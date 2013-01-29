pro nrs_scale_offset, img_name, outname = outname $
                    , scale = scale, offset = offset, off_before_scale = offset_before_scale $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  if n_elements(scale) eq 0 && n_elements(offset) eq 0 then begin
    void = dialog_message('Nothing to do: at least on of scale and offset should be specified', /error)
    return
  endif
  
  if n_elements(scale) eq 0 then scale = 1.0
  if n_elements(offset) eq 0 then offset = 0.0
  obs = keyword_set(offset_before_scale)
  
  envi_open_file, img_name, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq - 1 then begin
    void = dialog_message('Image open failed / not found', /error)
    return
  endif

  envi_file_query, fid, data_ignore_value = nodata, ns = ns, nl = nl, nb = nb $
                 , dims = dims $
                 , xstart = xs, ystart = ys $
                 , bnames = bnames, data_type = dt
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then delvar, mi
  
  dtx = where(dt eq [1 ,2 ,3 ,4 ,5 ,6 ,9 ,12 ,13 ,14 ,15], dt_cnt)
  
  if dt_cnt eq 0 then begin
    void = dialog_message('Image is not numeric', /error)
    return
  endif

  cancelled = 0

  if n_elements(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(img_name, postfix = '_offsca', ext = '.')
  endif

  nrs_set_progress_property, prog_obj, /start, title = 'Applying scale / offset'
  
  ; open the output for writing
  openw, unit, outname, /get_lun
  out_data = assoc(unit, make_array(ns, nl, type = dt))  ; bsq

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
    data = envi_get_data(fid = fid, dims = dims, pos = b)
    ix = where(data eq nodata, count)
    if obs then begin
      ; offset before scale
      data += offset
      data *= scale
    endif else begin
      data *= scale
      data += offset
    endelse
    
    if count gt 0 then data[ix] = nodata
    
    out_data[b] = data
  endfor
  close, unit
  free_lun, unit  ; close assoc
  
  envi_setup_head, fname = outname $
        , data_type = dt $
        , /write $
        , xstart = xs, ystart = ys $
        , interleave = 0 $  ; BSQ
        , nb = nb, nl = nl, ns = ns $
        , bnames = bnames $
        , map_info = mi $
        , data_ignore_value = nodata
end
