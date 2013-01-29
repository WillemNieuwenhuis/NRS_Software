pro nrs_normalize_index, image_name $
                       , outname = outname $
                       , ndvi_per_year = ndvi_per_year $
                       , per_year = per_year, multi_year = multi_year $
                       , percentage = percentage $
                       , from_max = from_max $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  envi_open_file, image_name, r_fid = fid, /no_realize, /no_interactive_query
  
  if fid eq -1 then return
  
  nrs_set_progress_property, prog_obj, title = 'Normalizing series', start = start
  cancelled = 0
  
  if n_elements(ndvi_per_year) eq 0 then ndvi_per_year = 36
  doPerYear = n_elements(per_year) gt 0
  doMulti = n_elements(multi_year) gt 0
  if doMulti then doPerYear = 0
  if (doMulti and doPerYear) eq 0 then doMulti = 1
  max_opt = (n_elements(from_max) gt 0) and (from_max eq 1) 
  
  if n_elements(percentage) eq 0 then percentage = 1
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nr_years = fix(nb / ndvi_per_year)
  if doMulti then begin
    nr_years = 1
    ndvi_per_year = nb
  endif
 
  if strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(image_name, ext = '.', postfix = '_norm')
  openw, unit, outname, /get_lun
  holder = assoc(unit, make_array(nb, ns, type = dt))  ; bip

  catch, stat
  if stat ne 0 then begin
    ; cleanup if some error
    close, unit  ; close assoc
    free_lun, unit
    file_delete, outname, /noexpand_path, /allow_nonexistent, /quiet
    catch, /cancel
    ans = dialog_message(!error_state.msg, title = 'Error', /error)
    return
  endif
  
  for line = 0, nl - 1 do begin
    cube = envi_get_slice(fid = fid, /bil, line = line, xs = 0, xe = ns - 1)
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    for y = 0, nr_years - 1 do begin
      sb = ndvi_per_year * y
      eb = sb + ndvi_per_year - 1
      for p = 0, ns - 1 do begin
        pixar = cube[p, sb : eb]
        ix = where(pixar ne undef, count)
        if count eq 0 then ix = lindgen(ndvi_per_year)
          
        masked = pixar[ix] 
        ndmin = min(masked, max = ndmax)
        if max_opt then begin
          pixar[ix] = (ndmax - pixar[ix]) / (ndmax - ndmin)
        endif else begin
          pixar[ix] = (pixar[ix] - ndmin) / (ndmax - ndmin)
        endelse
        if percentage eq 1 then begin
          pixar[ix] = pixar[ix] * 100.0
        endif
    
        cube[p, sb : eb] = pixar
      endfor
    endfor
    holder[line] = cube  ; bil storage
  endfor
  
  envi_setup_head, fname = outname, data_type = dt, /write $
          , interleave = 1 $  ; BIL
          , nb = nb, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , xstart = xs, ystart = ys $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close assoc
end

pro nrs_drought_index, vci_image, tci_image $
                       , outname = outname $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, vci_image, r_fid = fid_vci, /no_realize, /no_interactive_query
  if fid_vci eq -1 then return
  
  envi_open_file, tci_image, r_fid = fid_tci, /no_realize, /no_interactive_query
  if fid_tci eq -1 then return
  
  cancelled = 0
  
  envi_file_query, fid_vci, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , fname = image_name $
                 , data_ignore_value = undef
  mi = envi_get_map_info(fid = fid_vci, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  if strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(image_name, ext = '.', postfix = '_vtci')

  vcidata = envi_get_data(fid = fid_vci, dims = dims, pos = 0)
  tcidata = envi_get_data(fid = fid_tci, dims = dims, pos = 0)
  
  outdata = 0.7 * vcidata + 0.3 * tcidata
  
  envi_write_envi_file, outdata, out_name = outname, data_type = dt, bnames = ['drought index'] $
          , map_info = mi $
          , xstart = xs, ystart = ys $
          , data_ignore_value = undef
end
