;+
; :description:
;    Calculate an aggregated spectral profile for all locations in the input features.
;    The output is a text table organised with one spectrum for each location in a column. 
;    
;
; :params:
;    pnt_tbl : in, required
;      The input file with the points features
;    image : in, required
;      The input stack with the spectral information
;
; :keywords:
;    ignore_value : in, optional, default is empty
;      User specified value to handle as nodata value. Overrules the data provided nodata value!
;    debug : in, optional, default = off
;      Generate additional output: per location output a csv table with the all 
;      spectral profiles relevant for the aggregation
;    aggr_func : in, optional, default = average
;      The function to use for the aggregation; 
;    kernel : in, optional, default = 3
;      The size of the window (in pixels) around the location to include in the aggregation.
;      Kernel size can be [1 .. 11].
;    kern_type: in, optional, default = 0
;      The type of kernel area: 0 = square; 1 = circle
;      Note this will sample spectra around the central pixel, either as a square or as a circle,
;      but the distances are calculated assuming all pixel are the same size
;    xytable : in, optional, default = no
;      If true (yes) then the output will be an XY table with one location and spectrum
;      per row. If false (no) then the table will be organised with one location
;      and spectrum per column.
;    outname : out, optional
;      The name of the output table (CSV); if not specified will be
;      derived from the input image name
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :author: nieuwenhuis
; :history:
;   - jan 2014 - created
;   - jan 2015 - now allows kernel sizes from 1 (single profile) to 11
;   - aug 2015 - added kern_type keyword, allowing square or circular spectral sampling
;   - jan 2020 - added ignore_value, debug keywords
;-
pro nrs_aggregate_spectra, pnt_tbl, image $
                         , ignore_value = ignore_value $
                         , allow_undef = allow_undef $  ; for cases where input contains undefs as valid values
                         , debug = debug $      ; not available in GUI, defaults to 0 (== No)
                         , aggr_func = aggr_func $
                         , kernel = kernel, kern_type = kern_type $
                         , xytable = xytable $
                         , outname = outname $
                         , batch_mode = batch_mode $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  if n_elements(kernel) eq 0 then kernel = 3 $
  else kernel = min([11, max([1, kernel])])  ; kernel = 1, 3, 5, 7, 9, 11
  kern2 = fix(kernel / 2)
  if n_elements(kern_type) eq 0 then kern_type = 0 ; square

  if n_elements(aggr_func) eq 0 then aggr_func = 'mean'
  aggr_functions = ['min', 'max', 'mean', 'median']
  aggr_ix = where(strlowcase(aggr_func) eq aggr_functions, cnt)
  if cnt eq 0 then begin
    if ~keyword_set(batch_mode) then $
      void = error_message('Unsupported aggregation function')
    return
  endif

  if kernel eq 1 then aggr_ix = 99  ; just single profile in kernel

  cancelled = 0
  
  envi_open_file, image, r_fid = mapID, /no_realize, /no_interactive_query
  envi_file_query, mapID, nb = nb, nl = nl, ns = ns, data_ignore_value = undef, bnames = bnames, data_type = dt
  mi = envi_get_map_info(fid = mapID, undefined = undef_csy)
  if undef_csy eq 1 then begin
    if ~keyword_set(batch_mode) then $
      void = error_message('Spectral image has no coordinates')
    return
  endif
  has_undef = (size(undef, /type) eq dt) or ( (size(undef, /type) eq 5) and undef ne 1e34)
  if n_elements(ignore_value) gt 0 then begin
    has_undef = 1
    undef = fix(ignore_value, type = dt)
  endif
  
  ext = nrs_get_file_extension(pnt_tbl)
  if strlowcase(ext) eq '.shp' then begin
    crd = nrs_read_shape_points(pnt_tbl, hint_geo = isGeo)
    if n_elements(crd) eq 0 then begin
      if ~keyword_set(batch_mode) then $
        void = error_message('No points found')
      cancelled = 1
      return
    endif
    x = crd.x
    y = crd.y
  endif else begin
    nrs_read_points_csv, pnt_tbl, x, y, hint_geo = isGeo
    if n_elements(x) eq 0 then begin
      if ~keyword_set(batch_mode) then $
        void = error_message('No points found')
      cancelled = 1
      return
    endif
  endelse
  
  pointCount = n_elements(x)

  if (undef_csy eq 0 && mi.proj.type ne 0) && isGeo then begin
    geo = envi_proj_create(/geographic)
    envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
    envi_convert_file_coordinates, mapID, pixelX, pixelY, crd_x, crd_y
  endif else begin
    ; assume the same coordinate system
    envi_convert_file_coordinates, mapID, pixelX, pixelY, x, y
  endelse
  pixelX = round(pixelX)
  pixelY = round(pixelY)

  profiles = fltarr(pointCount, nb)
  valid_profiles = bytarr(pointCount)
  
  nrs_aggregate_spectra_select, kernel, kern_type, select = select
  index = [0, reform(total(select[0, *], /cum, /preserve), kernel)]
  specTotal = fltarr(total(select[0, *]), nb)
  nrs_set_progress_property, prog_obj, /start, title = 'Aggregate spectrum profile'
  for i = 0, pointCount - 1 do begin
    if nrs_update_progress(prog_obj, i, pointCount, cancelled = cancelled) then return

    ; only points located inside the bounds of the image can be processed
    if nrs_check_bounds(pixelX[i] - kern2, pixelY[i] - kern2, nl, ns) eq 0 then continue
    if nrs_check_bounds(pixelX[i] + kern2, pixelY[i] + kern2, nl, ns) eq 0 then continue

    ; collect the entire kernel
    specTotal[*] = 0
    lines = indgen(kernel) - kern2
    for l = 0, kernel - 1 do begin
      line = pixelY[i] + lines[l]
      xs = pixelX[i] + select[1, l]
      xe = pixelX[i] + select[2, l]
      specTotal[index[l] : index[l + 1] - 1, *] = envi_get_slice(fid = mapID, line = line, xs = xs, xe = xe, /bil)
    endfor
    
    allow_undef = 1 ; for debug only, comment out or remove during normal operation
    if ~allow_undef then begin
      if has_undef then begin
        ix = where(specTotal eq undef, ucnt)
        if ucnt gt 0 then specTotal[ix] = !values.F_NAN
      endif
    endif

    case aggr_ix of
      0  : profiles[i, *] = min(spectotal, dim = 1, nan = has_undef)
      1  : profiles[i, *] = max(spectotal, dim = 1, nan = has_undef)
      2  : profiles[i, *] = mean(spectotal, dim = 1, nan = has_undef)
      3  : profiles[i, *] = median(spectotal, dim = 1)  ; median automatically discards NaN values
      99 : profiles[i, *] = spectotal   ; single profile, just copy
    endcase
    valid_profiles[*] = 1
    if ~allow_undef then begin
      check = total(profiles[i, *])
      valid_profiles[i] = check gt 0
    endif
    
    if keyword_set(debug) then begin
      basename = 'debug' + string(i, format = '(i03)')
      debug_outname = getOutname(image, basename = basename, postfix = '', ext = '.csv')
      write_csv, debug_outname, specTotal
    endif
  endfor
  ix = where(valid_profiles eq 1, cnt)
  if cnt eq 0 then begin
    if ~keyword_set(batch_mode) then $
      void = error_message('None of the points (including kernel) overlap image')
    return
  endif
  index = make_array(pointCount, type = size(x, /type))
  index[*] = indgen(pointCount) + 1
  if keyword_set(xytable) then begin
    dig = fix(alog10(nb)) + 1
    if n_elements(bnames) ne nb then begin
      form = '("band_",' + string(dig, format = '("i0", i0)') + ')'
      bnames = string(indgen(nb) + 1, format = form)
    endif
    hdr = ['Index', 'X', 'Y', bnames]
    profiles = [transpose(index[ix]), transpose(x[ix]), transpose(y[ix]), transpose(profiles[ix, *])]
  endif else begin
    hdr = string([transpose(x[ix]), transpose(y[ix])], format = '("(",f0.6,":",f0.6,")")')
    profiles = transpose([transpose(index[ix]), transpose(profiles[ix, *])])
  endelse
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_prof', ext = '.csv')
  
  if cnt eq 1 then profiles = reform(profiles, n_elements(hdr), 1, /overwrite)
  write_csv, outname, header = hdr, profiles
end
