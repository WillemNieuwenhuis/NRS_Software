;+
; :description:
;    Calculate the 95th percentile value of the wet days (> 1mm precipitation)
;    in the period 1961-1990.
;
; :params:
;    inname : in
;      daily timeseries from 1961-1990 with precipitation values
;
; :keywords:
;    outname : in
;      The output name of the p95 image
;    zhang : in, optional, default = no
;      If set implements the percentile procedure as proposed by Zhang ea.
;      If not set will simply calculate the percentile coventionally
;    prog_obj : in, optional
;      Progressbar object to indicate progress of the calculation
;    cancelled : in, optional
;      If a progressbar is used, signals that the user stopped the calculation
;
; :reference:
;   Zhang, Xuebin, et al. "Avoiding inhomogeneity in percentile-based indices of temperature extremes." Journal of Climate 18.11 (2005): 1641-1651.
; :author: nieuwenhuis
; :History:
;   :code changes::
;     sept 2013 - created
;-
pro nrs_climind_percentiles, inname $
           , outname = outname $
           , zhang = zhang $
;           , startday, endday $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  if keyword_set(zhang) then begin
    nrs_climind_percentiles_zhang, inname, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  endif else begin
    nrs_climind_percentiles_native, inname, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  endelse 
end

pro nrs_climind_percentiles_native, inname $
           , outname = outname $
           , zhang = zhang $
;           , startday, endday $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate, hidden
  
  cancelled = 1

  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Could not open input timeseries')
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nrs_set_progress_property, prog_obj, /start, title = 'Climate indices'

  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then begin
    outname = getOutname(inname, postfix = '_hpct', ext = '.dat')
  endif

;  if ~((n_elements(undef) gt 0) && (undef ne 1e34)) then undef = -9999

  startday = nrs_str2julian('1-1-1961')
  endday = nrs_str2julian('31-12-1990')
  if nb ne (endday - startday + 1) then begin
    void = error_message('Probably not daily data, quitting')
    return
  endif

  cancelled = 0
  
  limit = 1.0 ; in mm (only deal with wet days)

  openw, unit, outname, /get_lun
  
  out_data = intarr(ns, nl)

  jm -= startday
  pos = indgen(nb)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)

    for col = 0, ns - 1 do begin
      series = slice[col, *]
      ix = where(series ge limit, cnt)
      if cnt eq 0 then begin
        out_data[col, line] = 0.0
        continue
      endif
       
      series = series[ix]
      ix = sort(series)
      out_data[col, line] = series[ix[long(0.95 * cnt)]]
    endfor
    
    writeu, unit, out_data
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  dt = size(out_data, /type)
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = 1, nl = nl, ns = ns $
          , bnames = '95th Percentile' $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file
end

pro nrs_climind_percentiles_zhang, inname $
           , outname = outname $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate, hidden
  
  cancelled = 1

  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Could not open input timeseries')
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nrs_set_progress_property, prog_obj, /start, title = 'Climate indices'

  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then begin
    outname = getOutname(inname, postfix = '_hpct', ext = '.dat')
  endif

;  if ~((n_elements(undef) gt 0) && (undef ne 1e34)) then undef = -9999

  startday = nrs_str2julian('1-1-1961')
  endday = nrs_str2julian('31-12-1990')
  caldat, [startday, endday], mm, dd, yy
  sy = yy[0]
  ey = yy[1]
  nr_years = ey - sy + 1 
  nrs_get_dt_indices, [startday, endday + 1], period = 'year', julian_out = jm
  
  if nb ne (endday - startday + 1) then begin
    void = error_message('Probably not daily data, quitting')
    return
  endif

  cancelled = 0
  
  limit = 1.0 ; in mm (only deal with wet days)

  openw, unit, outname, /get_lun
  
  out_data = intarr(ns, nl)

  jm -= startday
  pos = indgen(nb)
  p95 = long(0.95 * nb)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)

    for col = 0, ns - 1 do begin
      series = slice[col, *]
      ix = where(series ge limit, cnt)
      if cnt eq 0 then begin
        out_data[col, line] = 0.0
        continue
      endif
      
      oob = series[jm[0] : jm[1] - 1] ; use first year as out of base year
      block = series
      estim = fltarr(nr_years)  ; intermediate p95 values
      for y = 1, nr_years - 1 do begin
        ps = jm[y]
        pe = jm[y + 1] - 1
        block[jm[0] : jm[1] - 1] = series[ps : pe] ; mutate block by duplicating current year
        
        ; now find 95th percentile of the block
        ix = sort(block)
        p = block[ix[p95]]
        
        ; calculate the r95p value for the out-of-base data
        ix = where(oob gt p, cnt)
        estim[y] = cnt ne 0 ? total(oob[ix]) : 0.0
      endfor
      
      out_data[col, line] = mean(estim[1:nr_years])
    endfor
    
    writeu, unit, out_data
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  dt = size(out_data, /type)
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = 1, nl = nl, ns = ns $
          , bnames = '95th Percentile' $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file
end
