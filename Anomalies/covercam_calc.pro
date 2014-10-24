pro covercam_calc, inputfile, refimage, classfile, ndvi_py, sd_mult, pixmask $
                , fromtime, totime, sel_year $
                , abs_diff $
                , magname $
                , prog_obj = progressBar, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  nrs_set_progress_property, progressBar, /start, title = 'Detect NDVI changes'
  
  nodata = -9999.0
  
  ; open inputs
  envi_open_file, inputfile, r_fid = ndvi, /no_realize, /no_interactive_query
  if ndvi eq -1 then return
  
  envi_open_file, refimage, r_fid = ndvi_ref, /no_realize, /no_interactive_query
  if ndvi_ref eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass
  
  envi_file_query, ndvi, dims = dims, ns = ns, nl = nl, nb = nrlayers
  mi_ref = envi_get_map_info(fid = ndvi, undefined = undef_csy)
  if undef_csy then void = temporary(mi_ref)
  
  envi_file_query, ndvi_ref, ns = ns_ref, nl = nl_ref
  envi_file_query, class, ns = ns_class, nl = nl_class
  
  dim_ok = (ns eq ns_ref) && (ns eq ns_class) && (nl eq nl_ref) && (nl eq nl_class)
  if dim_ok eq 0 then begin
    void = dialog_message('Check images: dimensions are not the same' $
      , title = 'Probability of change' $
      , /error)
    return
  endif

  cancelled = 0
  
  ndvi_py_org = ndvi_py
  nb = nrlayers
  nryears = nrlayers / ndvi_py
  
  ndvi_py = totime - fromtime + 1
  if sel_year eq -1 then sel_year = 1 else nryears = 1
  sel_year -= 1
  nrlayers = ndvi_py * nryears
  
  ;  allocate memory for output
  magdata = fltarr(ns * nl, nryears)
  
  ; allocate memory for loop
  yearmag = fltarr(ns * nl, ndvi_py)
  
  mag_layer = fltarr(ns * nl)
  
  ; create segments, and the mask to remove small areas
  nrs_set_progress_property, progressBar, title = 'Finding segments'
  nrs_area_numbering_data, cldata, areas = segdata, undef = undef, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  ix = where(segdata eq undef, count)
  nrs_set_progress_property, progressBar, /start, title = 'Build mask'
  nrs_build_mask_by_area, segdata, area = pixmask, mask = mask, undef = undef, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  mask = reform(mask, ns, nl, /overwrite)
  
  ; calculate the SD LUT from the historical data
  nrs_set_progress_property, progressBar, /start, title = 'Calculate pooled LUT'
  nrs_calc_class_stddev, ndvi_ref, class, stdevs = stdevs, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  nrs_calc_pooled_sd, stdevs, ndvi_py, poolsd = poolsd
  
  lut = poolsd * sd_mult
  
  ; base calculation: find differences with averages per class
  nrs_set_progress_property, progressBar, /start, title = 'Detect NDVI changes'
  for y = 0, nryears - 1 do begin
    yearmag[*] = 0.0
    ; calculate anomalies and magnitudes for an entire year
    for lpy = 0, ndvi_py - 1 do begin
      layer = y * ndvi_py + lpy
      inp_layer = (y + sel_year) * ndvi_py_org + (lpy + fromtime - 1)
      if nrs_update_progress(progressBar, layer, nrlayers) then begin
        return
      endif
      ldata = envi_get_data(fid = ndvi, dims = dims, pos = inp_layer)
      
      nrs_ndvi_magdata, ldata, cldata, segdata, lut, ndvi_py, lpy, magdata = mag_layer, abs_diff = abs_diff
      
      yearmag[*, lpy] = mag_layer
    endfor
    
    ; indicate yearly (period) anomaly
    for p = 0L, ns * nl - 1L do begin
      magdata[p, y] = total(yearmag[p, *])
    endfor
  endfor
  
  ; now apply mask, if needed
  if pixmask gt 0 then begin
    ix = where(mask eq 0, count)
    if count gt 0 then begin
      magdata[ix, *] = nodata
    endif
  endif
  magdata = reform(magdata, ns, nl, nryears, /overwrite)
  
  out_mag = getoutname(magname, postfix = '', ext = '.dat')
  
  ; build band names
  nn = indgen(nrlayers)
  dnames = 'Year.nr ' + string(sel_year + 1 + nn / ndvi_py, format = '(I0)') + '.' + string(fromtime + nn mod ndvi_py, format = '(I0)')
  bc = indgen(nryears) + sel_year + 1
  bnames = string(bc, format = '("Year ",I02)')
  mnames = string(bc, format = '("Magnitude (year ",I02,")")')
  
  envi_write_envi_file, magdata, out_name = out_mag, bnames = mnames, map_info = mi_ref $
    , data_ignore_value = nodata
    
end  
