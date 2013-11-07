pro nrs_find_growth_start, image, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  nrs_set_progress_property, prog_obj, title = 'Find growth start', /start

  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, nb = nb, nl = nl, ns = ns, dims = dims $
                      , data_type = dt, data_ignore_value = undef_loc
  mi = envi_get_map_info(fid = fid, undefined = undefined)

  cancelled = 0
  
  cap_cnt = 9
  out_data = make_array(ns, cap_cnt + 2, type = dt)
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_grow', ext = '.dat')
  openw, unit, outname, /get_lun
  
  pos = indgen(nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos)
    out_data[*] = 0
    for s = 0, ns - 1 do begin
      b = 0
      while (b lt nb) && (data[s, b] gt 0) do b++
      if b ge nb then b = 0 $ ; no zeroes found
      else begin
        while (b lt nb) && (data[s, b] eq 0) do b++
        if b ge nb then continue  ; only zeroes found
      endelse
      last = (b + cap_cnt) < nb
      max_sel = indgen(last - b) + b
      d = reform(data[s, max_sel], n_elements(max_sel))
      mx_ndvi = max(d, mx_ix)
      out_data[s, 0 : n_elements(max_sel) + 1] = [b + 1, mx_ndvi, d]  
    endfor
    
    writeu, unit, out_data
  endfor

  close, unit
  free_lun, unit
  
  bands = string(indgen(9) + 1, format = '("NDVI value ",i0)')
  bnames = ['Start of growth', 'Max NDVI', bands]
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , nb = 11 $
          , nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , interleave = 1 $  ; BIL = 1
          , data_ignore_value = undef
  
end