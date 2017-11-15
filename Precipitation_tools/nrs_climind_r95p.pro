pro nrs_climind_r95p, inname $
           , perc_image $
           , outname = outname $
           , startday, endday $
           , calc_fraction = calc_fraction $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1

  calc_fraction = keyword_set(calc_fraction)
  
  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Could not open input timeseries')
    return
  endif
  
  envi_open_file, perc_image, r_fid = fid_p, /no_realize, /no_interactive_query
  if fid_p eq -1 then begin
    void = error_message('Could not open percentile image')
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nrs_set_progress_property, prog_obj, /start, title = 'Climate indices'

  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then begin
    outname = getOutname(inname, postfix = '_r95p', ext = '.dat')
  endif

  caldat, [startday, endday], mm, dd, yy
  sy = yy[0]
  ey = yy[1]
  nr_years = ey - sy + 1 
  nrs_get_dt_indices, [startday, endday + 1], period = 'year', julian_out = jm
  
  if nb ne (endday - startday + 1) then begin
    void = error_message('Probably not daily data, quitting', /error)
    return
  endif

  cancelled = 0
  
  limit = 1.0 ; in mm (only deal with wet days)

  openw, unit, outname, /get_lun
  if calc_fraction then begin
    out_f = getOutname(inname, postfix = '_frac', ext = '.dat')
    openw, unit_f, out_f, /get_lun
  endif
  
  bn = indgen(nr_years) + sy
  bnames = [string(bn, format = '(i0)')]
    
  out_data = intarr(ns, nr_years)
  out_fraction = fltarr(ns, nr_years)

  jm -= startday
  pos = indgen(nb)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    p95 = envi_get_slice(fid = fid_p, line = line, xs = 0, xe = ns - 1, pos = [0], /bil)
    q95 = rebin(p95, ns, nb)  ; duplicate in band dir to get same size as input slice
    
    ix = where(slice lt limit, cnt)
    if cnt eq nb then out_data[*] = 0.0 $
    else begin
      above = (slice gt q95)
      data = above * slice  ; anything less than threshold -> 0
      data[ix] = 0 ; exclude dry days, by setting precipitation on those days to zero
      
      for y = 0, nr_years - 1 do begin
        ps = jm[y]
        pe = jm[y + 1] - 1
        p_above = total(data[*, ps : pe], 2) ; total annual precipitation on stormy days (p > threshold)
        pt = total(slice[*, ps : pe], 2)  ; total annual precipitation
        out_data[*, y] = p_above
        out_fraction[*, y] = 100.0 * p_above / pt
      endfor
    endelse
    
    writeu, unit, out_data
    if calc_fraction then writeu, unit_f, out_fraction
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  dt = size(out_data, /type)
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nr_years, nl = nl, ns = ns $
          , bnames = bnames $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file

  if calc_fraction then begin
    dt = size(out_fraction, /type)
    envi_setup_head, fname = out_f $
      , data_type = dt $
      , /write $
      , interleave = 1 $  ; BIL
      , nb = nr_years, nl = nl, ns = ns $
      , bnames = bnames $
      , inherit = meta $
      , data_ignore_value = undef
  
    close, unit_f
    free_lun, unit_f  ; close output file
  
  endif
end
