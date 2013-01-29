function nrs_complex2polar, cnum, degrees = degrees
  a = real_part(cnum)
  b = imaginary(cnum)
  
  angles = atan(b / a, /phase)

  for i = 0, n_elements(a) - 1 do begin
    if a[i] eq 0 && b[i] eq 0 then angles[i] = !values.f_nan
    if a[i] eq 0 && b[i] gt 0 then angles[i] = !pi / 2
    if a[i] eq 0 && b[i] lt 0 then angles[i] = -!pi / 2
    if a[i] lt 0 && b[i] gt 0 then angles[i] += !pi
    if a[i] lt 0 && b[i] lt 0 then angles[i] -= !pi
  endfor
  doDegrees = ~(n_elements(degrees) eq 0 || degrees eq 0)
  if doDegrees then angles *= !radeg
  
  return, [abs(cnum), angles]
end

function nrs_polar2complex, c_abs, c_ph
  dt = size(c_abs, /type)
  cmp_ar = complex(c_abs * cos(c_ph), c_abs * sin(c_ph), double = dt eq 5)
  
  return, cmp_ar
end

;+
; :Description:
;    Perform the harmonic analysis on a timeseries on a pixel by pixel basis.
;
; :Params:
;    image : in
;      the time series
;    max_harmonic : in
;      The maximum harmonic to analyse and store
;
; :Keywords:
;    outname : in
;      The name of the output
;    img_py : in, default = 36
;      Number of layers per year
;    degrees : in
;      If set output angles in degrees, otherwise as radians
;    prog_obj : in
;      A progress indicator object. If specified causes the progress to be displayed
;    cancelled : out
;      Will be set if the progress is interrupted by the user 
;
; :Author: nieuwenhuis
;-
pro nrs_harmonic_analysis, image, max_harmonic, outname = outname, img_py = img_py, degrees = degrees, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return

  cancelled = 0
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then delvar, mi
  
  if n_elements(img_py) eq 0 then img_py = 36
  doDegrees = ~(n_elements(degrees) eq 0 || degrees eq 0)
  mh = min([max_harmonic, img_py])
  nrs_set_progress_property, prog_obj, /start, title = 'Harmonic analysis'
  
  nr_years = fix(nb / img_py[0])
  
  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then $
    outname = getOutname(image, postfix = '_har', ext = '.')
  openw, unit, outname, /get_lun
  nbo_py = mh * 2 + 2 ; mean, mh * (amplitude, phase), rmse
  out_data = assoc(unit, fltarr(ns, nr_years * nbo_py))  ; bil

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  line_dat = fltarr(ns, nr_years * nbo_py)
  ccz = complex(0, 0, double = dt eq 5)
  fft_ix = indgen(img_py - nbo_py + 1) + mh + 1
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, /bil, line = l, xs = 0, xe = ns - 1)
    for s = 0, ns - 1 do begin
      if slice[s, 0] eq undef then continue ; skip undefined

      for y = 0, nr_years - 1 do begin
        ; FFT
        si = y * img_py
        ei = si + img_py - 1
        spec = slice[s, si : ei]
        four = fft(spec)
        polar = nrs_complex2polar(four[1 : mh], degrees = doDegrees)
        ; calculate RMSE
        four[fft_ix] = ccz
        inv = abs(fft(four, 1))
        rmse = sqrt(total((spec - inv) ^ 2) / n_elements(spec))
        ; output
        si = y * nbo_py
        ei = si + nbo_py - 2
        line_dat[s, si : ei] = [abs(four[0]), polar]
        line_dat[s, ei + 1] = rmse
      endfor
    endfor
    out_data[l] = line_dat
  endfor
  
  pfix = ['1st', '2nd', '3rd']
  for pp = 4, mh do pfix = [pfix, string(pp, format = '(i0,"th")')]
  if nr_years le 1 then begin
    bnames = ['Mean' + string(nb, format = '(" (",i0," bands per year)")') $
                    , string(pfix, format='("Amplitude ",a0, " harmonic")') $
                    , string(pfix, format='("Phase ",a0, " harmonic")') $
                    , 'RMSE']
  endif else begin
    bnames = []
    for y = 1, nr_years do begin
      bnames = [bnames, string(y, format = '("Mean (year ", i0, ")")') $
                      , string(pfix, format='("Amplitude ",a0, " harmonic")') $
                      , string(pfix, format='("Phase ",a0, " harmonic")') $
                      , 'RMSE']
    endfor
  endelse

  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nr_years * nbo_py, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close assoc
end

;+
; :Description:
;    Extract harmonic polar values and convert them to complex numbers. Then setup
;    the array for reverse FFT by setting the conjugate complex numbers.  
;
; :Params:
;    slice : in
;      The input polar values (all columns)
;    nr_harm : in
;      The number of harmonics
;    usedDegrees : in
;      If the values are stored as degrees, first convert them back to radians
;    cmp_ar : out
;      The array with the complex values; it is organised as:
;      <pre>
;      [ C0, C1, C2, ...Cn, 0, 0, 0,..., 0, 0, 0, conj(Cn),..., conj(C2), conj(C1)]
;      </pre>
;      where:
;      <ul>
;      <li>C0   : mean
;      <li>Ci   : complex numbers as converted from file
;      <li>conj(Ci) : conjugates of the complex numbers Ci
;      <li>n    : The number of harmonics stored in the file
;      </ul> 
;
; :Author: nieuwenhuis
;-
pro nrs_harm_to_complex, slice, nr_harm, usedDegrees, cmp_ar
  cmp_ar[*, 0] = complex(slice[*, 0]) ; mean
  ix = indgen(nr_harm) + 1  ; index to amplitude
  ixp = ix + nr_harm        ; index to phase
  if usedDegrees then slice[*, ixp] *= !dtor
  ixi = ix + n_elements(cmp_ar[0, *]) - nr_harm - 1
  cmp_ar[*, ix] = nrs_polar2complex(slice[*, ix], slice[*, ixp])
  cmp_ar[*, ixi] = conj(cmp_ar[*, nr_harm - ix + 1])
end

function nrs_harm_check_use_degrees, fid, nr_harm
  envi_file_query, fid, ns = ns, nl = nl, data_ignore_value = undef
  slice = envi_get_slice(fid = fid, /bil, line = nl/2, xs = 0, xe = ns - 1)
  ix = where(slice ne undef, nr_ok)
  if nr_ok eq n_elements(slice) then begin
    mx = max(slice[*, indgen(nr_harm) + nr_harm + 1]) ; read the phase values
  endif else begin
    ix = array_indices(slice, ix)
    ix = ix[0, *]
    cix = ix[uniq(ix, sort(ix))]
    mx = max(slice[ix, indgen(nr_harm) + nr_harm + 1]) ; read the phase values
  endelse
  return = mx gt 2 *!pi
end

;+
; :Description:
;    Perform the harmonic composition of harmonic analysis.
;
; :Params:
;    image : in
;      the Harmonic analysis result
;    nr_years : in
;      The number of years of data in the input
;
; :Keywords:
;    outname : in
;      Name of the output
;    img_py : in, default = 36
;      Number of bands in the timeseries / spectrum to reconstruct
;      If possible it is extracted from the metadata of the input; if not found or
;      specified, it defaults to 36 (10 day aggregate)
;    harmonics : in, default = 3
;      The number of harmonics (per year) in the input data; if not specfied
;      defaults to 3.
;    prog_obj : in
;      A progress indicator object. If specified causes the progress to be displayed
;    cancelled : out
;      Will be set if the progress is interrupted by the user 
;
; :Author: nieuwenhuis
;-
pro nrs_harmonic_composition, image, outname = outname $
                            , nr_years, harmonics = harmonics $   ; input data parameters
                            , img_py = img_py $                   ; output data parameters
                            , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  if n_elements(img_py) eq 0 || img_py lt 12 then img_py = 36
  if n_elements(harmonics) eq 0 then harmonics = 3

  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then delvar, mi
  
  nbi_py = 2 * harmonics + 1
  hasRMSE = (nb - nr_years * nbi_py) eq nr_years
  nr_harm = harmonics
  nb_py = nb / nr_years
  im = where(strpos(bnames, 'Mean') eq 0, nr_years_alt)
  ih = where(strpos(bnames, 'Amplitude') eq 0, nr_harm_alt)
  ip = where(strpos(bnames, 'Phase') eq 0, nr_phase)
  ir = where(strpos(bnames, 'RMSE') eq 0, nr_rmse)
  if (nr_years_alt gt 0) && (nr_years ne nr_years_alt) then begin
    msg = 'Input parameters don''t match the input harmonic decomposition'
    ans = error_message(msg + ' cannot continue', title = 'Harmonic composition', /error)
    return
  endif
  if (nr_harm_alt gt 0) && (nr_harm ne nr_harm_alt / nr_years) then begin
    msg = 'Input parameters don''t match the input harmonic decomposition'
    ans = error_message(msg + ' cannot continue', title = 'Harmonic composition', /error)
    return
  endif
  cancelled = 0
  
  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then $
    outname = getOutname(image, postfix = '_hcom', ext = '.')
  openw, unit, outname, /get_lun
  out_data = assoc(unit, fltarr(ns, nr_years * img_py))  ; bil

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  nrs_set_progress_property, prog_obj, /start, title = 'Harmonic composition'
  cmp_ar = complexarr(ns, img_py)
  line_dat = fltarr(ns, nr_years * img_py)
  usedDegrees = nrs_harm_check_use_degrees(fid, nr_harm)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, /bil, line = l, xs = 0, xe = ns - 1)
    for y = 0, nr_years - 1 do begin
      si = y * nb_py    ; also skip rmse if needed
      ei = si + nbi_py - 1
      nrs_harm_to_complex, slice[*, si : ei], nr_harm, usedDegrees, cmp_ar
      for s = 0, ns - 1 do begin
        hspec = cmp_ar[s, *]
        four = fft(hspec, 1)  ; inverse fft
        syi = y * img_py
        eyi = syi + img_py - 1
        line_dat[s, syi : eyi] = abs(four)
      endfor
      out_data[l] = line_dat
    endfor
  endfor
  
  lookup_days = [[12, 23, 36, 72] $   ; number of images per year
               , [30, 16, 10,  8]]    ; number of days per period
  lix = where(lookup_days[*, 0] eq img_py, cnt)
  bn = reform((indgen(img_py * nr_years) mod img_py) * lookup_days[lix[0], 1] + 1, 1, img_py * nr_years)
  yn = reform((indgen(img_py * nr_years) / img_py) + 1, 1, img_py * nr_years)
  bnames = string([yn, bn], format = '("year.doy ",i0,".",i03)')

  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nr_years * img_py, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi

  close, unit
  free_lun, unit  ; close assoc
  
end