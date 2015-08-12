pro nrs_pheno_extract_abcd, slice, ns = ns, nr_years = nr_years $
            , low_level, high_level $
            , l_min_val, l_min_ix, r_min_val, r_min_ix $
            , max_val $
            , doy_offsets $
            , ac = par_sos, bd = par_eos
  compile_opt idl2, logical_predicate

  dsos = l_min_val + (max_val - l_min_val) * low_level
  deos = r_min_val + (max_val - r_min_val) * high_level
  par_sos = intarr(ns, nr_years * 2)
  par_eos = intarr(ns, nr_years * 2)
  for s = 0, ns - 1 do begin
    for p = 0, nr_years * 2 - 1 do begin
      ixl = where(dsos[s, p] lt slice[s, p, *], cnt_left)
      ixr = where(deos[s, p] lt slice[s, p, *], cnt_right)
      ; sos and eos are calculated assuming monotonous curves!
      ; so take first min for "start of season"
      ; and take last min for "end of season"
      par_sos[s, p] = ixl[0] * 8 + doy_offsets[p] ; turn index into DOY
      par_eos[s, p] = ixr[-1] * 8 + doy_offsets[p]
    endfor
  endfor
  ; correct for overflow of days past end of year
  ; wrap them around: DOY = 366 becomes DOY = 1 etc
  par_sos = ((par_sos - 1) mod 365) + 1
  par_eos = ((par_eos - 1) mod 365) + 1
end

pro nrs_phenological_params, image, basename = basename, alevel = alevel, blevel = blevel, maxlevel = maxlevel $
                           , start_date = start_date, end_date = end_date $
                           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then return
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt
  inherit = envi_set_inheritance(fid, dims, /full)
  
  if abs(fix(nb / 46.0) - nb / 46.0) gt 0.1 then begin
    void = error_message('Band number is not a multiple of 46')
    return
  endif
  
  cancelled = 0
  
  img_py = 46           ; images per year
  img_ps = img_py / 2   ; images per season
  img_interval = 8      ; 8 days interval between images
  nr_years = nb / img_py
  doy_offsets = intarr(nr_years * 2) ; assume two seasons per year
  if n_elements(start_date) gt 0 then begin
    sd = nrs_str2julian(start_date)
    ed = nrs_str2julian(end_date)
    doy_offset = nrs_doy_from_julian(sd)
  endif else begin
    doy_offset = 1
  endelse
  doy_ix = indgen(nr_years)
  doy_offsets[doy_ix * 2] = doy_offset
  doy_offsets[doy_ix * 2 + 1] = doy_offset + img_ps * img_interval
  
  if n_elements(alevel) eq 0 then alevel = 0.1
  if n_elements(blevel) eq 0 then blevel = 0.1
  if n_elements(maxlevel) eq 0 then maxlevel = 0.9
  
  caldat, sd, mm, dd, yy
  yearstr = string(yy + indgen(nr_years * 2) / 2, format = '(i4)')
  pstr = string(1 + (indgen(nr_years * 2) mod 2), format = '("_",i02)')
  bnames = yearstr + pstr

  ; determine output filenames and open all
  if n_elements(basename) eq 0 then basename = image
  fnout_par_a = getOutname(basename, postfix = '_sos', ext = '.dat')
  fnout_par_b = getoutname(basename, postfix = '_eos', ext = '.dat')
  fnout_par_c = getoutname(basename, postfix = '_c', ext = '.dat')
  fnout_par_d = getoutname(basename, postfix = '_d', ext = '.dat')
  fnout_par_e = getoutname(basename, postfix = '_peak', ext = '.dat')
  fnout_par_f = getoutname(basename, postfix = '_amp', ext = '.dat')
  fnout_par_g = getoutname(basename, postfix = '_los', ext = '.dat')
  fnout_par_skew = getoutname(basename, postfix = '_skew', ext = '.dat')
  openw, fp_a, fnout_par_a, /get_lun  
  openw, fp_b, fnout_par_b, /get_lun
  openw, fp_c, fnout_par_c, /get_lun
  openw, fp_d, fnout_par_d, /get_lun
  openw, fp_e, fnout_par_e, /get_lun
  openw, fp_f, fnout_par_f, /get_lun
  openw, fp_g, fnout_par_g, /get_lun
  openw, fp_skew, fnout_par_skew, /get_lun

  bix = indgen(nb)
  l_min_ix = intarr(ns, nr_years * 2)
  r_min_ix = intarr(ns, nr_years * 2)
  l_min_val = fltarr(ns, nr_years * 2)
  r_min_val = fltarr(ns, nr_years * 2)
  for l = 0, nl - 1 do begin
    ; get slice in ns x nb
    slice = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = bix, /bil)
    ; reform to ns x nr_years * 2 x 23
    ; so split the years into two seasons
    slice = reform(slice, ns, nr_years * 2, img_ps, /overwrite)
    ; remember nan values for masking later
    nan_ix = where(finite(slice[*, 0, 0], /nan) eq 1, nan_cnt)
    
    max_val = max(slice, max_ix, dim = 3)
    fail = where(max_val le 0.4, fail_cnt)

    max_aix = reform(array_indices(slice, max_ix), 3, ns, nr_years * 2)
    ; the subscripts contains the band indices of the max values, turn them into doy
    par_e = max_aix[2, *, *] * img_interval ;+ doy_offsets
    ; correct for overflow of days past end of year
    ; wrap them around: DOY = 366 becomes DOY = 1 etc
    par_e = ((par_e - 1) mod 365) + 1  ; we found par_e

    max_pix = max_ix
    if fail_cnt gt 0 then begin
      ; indicate seasons without max
      par_e[fail] = 0
      max_ix[fail] = -1
      ; determine fake max in the middle of the season
      ; to be able to determine minimum values
      max_pix[fail] = lindgen(n_elements(fail)) * ns + ns / 2
    endif
    
    ; find the min value per season, before the mid of season (peak)
    for s = 0, ns - 1 do begin
      for p = 0, nr_years * 2 - 1 do begin
        cur_mx = max_aix[2, s, p]
        if cur_mx gt 0 then begin
          l_min_val[s, p] = min(slice[s, p, 0 : cur_mx], mx)
          l_min_ix[s, p] = mx
        endif else l_min_ix[s, p] = -1
      endfor
    endfor
    ; take the left minimum also for the right minimum of the previous season
    r_min_ix[*, 0 : -2] = l_min_ix[*, 1 : -1] ; right minimum index
    r_min_ix[*, -1] = l_min_ix[*, -1] ; duplicate last minimum
    r_min_val[*, 0 : -2] = l_min_val[*, 1 : -1] ; right minimum
    r_min_val[*, -1] = l_min_val[*, -1] ; duplicate last minimum

    ; determine start and end of season
    nrs_pheno_extract_abcd, slice, ns = ns, nr_years = nr_years $
              , alevel, blevel $
              , l_min_val, l_min_ix, r_min_val, r_min_ix $
              , max_val $
              , doy_offsets $
              , ac = par_sos, bd = par_eos

    ; determine c and d extraction values
    nrs_pheno_extract_abcd, slice, ns = ns, nr_years = nr_years $
              , maxlevel, maxlevel $
              , l_min_val, l_min_ix, r_min_val, r_min_ix $
              , max_val $
              , doy_offsets $
              , ac = par_c, bd = par_d

    par_sos = par_sos > 0 ; no negative sos (should not happen anyway!)
    par_f = max_val - (l_min_val + r_min_val) / 2
    par_f = par_f > 0.0 ; make sure par_f is not negative 
    par_skew = float(max_ix - l_min_ix) / (max_ix - r_min_ix)
    
    ; undo calculations for location with input NAN
    if nan_cnt gt 0 then begin
      par_sos[nan_ix, *] = -999
      par_eos[nan_ix, *] = -999
      par_c[nan_ix, *] = -999
      par_d[nan_ix, *] = -999
      par_e[nan_ix, *] = -999
      par_f[nan_ix, *] = -999
      par_g[nan_ix, *] = -999
      par_skew[nan_ix, *] = -999
    endif
    ; now save the timesat indices
    writeu, fp_a, fix(par_sos) 
    writeu, fp_b, fix(par_eos)
    writeu, fp_c, fix(par_c)
    writeu, fp_d, fix(par_d)
    writeu, fp_e, fix(par_e)
    writeu, fp_f, par_f
    writeu, fp_g, fix(par_eos - par_sos)
    writeu, fp_skew, par_skew
  endfor

  close, fp_a, fp_b, fp_c, fp_d, fp_e, fp_f, fp_g, fp_skew
  free_lun, fp_a, fp_b, fp_c, fp_d, fp_e, fp_f, fp_g, fp_skew
  
  types = [size(par_sos, /type), size(par_eos, /type), size(par_c, /type), size(par_d, /type) $
         , size(par_e, /type), size(par_f, /type), size(par_sos, /type), size(par_skew, /type)]
  undefs = [-999.0, -999.0, -999.0, -999.0, -999.0, -999.0, -999.0, -999.0]
  fns = [fnout_par_a, fnout_par_b, fnout_par_c, fnout_par_d, fnout_par_e, fnout_par_f, fnout_par_g, fnout_par_skew]
  interleave = 1 ; BIL
  for f = 0, n_elements(fns)- 1 do begin
    envi_setup_head, fname = fns[f] $
      , data_type = types[f] $
      , ns = ns, nl = nl, nb = nr_years * 2 $
      , interleave = interleave $
      , data_ignore_value = fix(undefs[f], type = types[f]) $
      , bnames = bnames $
      , /write $
      , inherit = inherit
  endfor
  
end

