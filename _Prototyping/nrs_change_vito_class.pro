pro nrs_change_vito_class, image, output
  compile_opt idl2, logical_predicate

  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb $
               , data_type = dt, bnames = bnames $
               , data_ignore_value = nodata $
               , num_classes = nclass $
               , lookup = lookup $
               , class_names = class_names
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  if undefined eq 1 then delvar, mi
  inherit = envi_set_inheritance(fid, dims, /full)
  
  patlut = [10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100]
  dout = make_array(ns, nl, type = dt)
  for l = 0, nl - 1 do begin
    data = envi_get_slice(fid = fid, xs = 0, xe = ns - 1, line = l, pos = 0)
    for p = 0, n_elements(patlut) - 1 do begin
      ix = where(data eq patlut[p], cnt)
      if cnt gt 0 then dout[ix, l] = p + 1
    endfor
  endfor
  
  envi_file_mng, id = fid, /remove  
  
  envi_write_envi_file, dout, out_name = output, inherit = inherit $
      , file_type = 'ENVI Classification' $
      , class_names = class_names $
      , num_classes = nclass $
      , lookup = lookup

end