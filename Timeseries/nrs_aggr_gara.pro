pro nrs_gara_aggr_main
  compile_opt idl2, logical_predicate

  filename = DIALOG_PICKFILE(title='Specify timeseries', file=filename, /read)
  if strlen(filename) eq 0 then return
  
  outname = DIALOG_PICKFILE(title='Sepcify output name', file=outname, /read)
  if strlen(outname) eq 0 then return
  
  nrs_aggr_gara, filename, outname
end

pro nrs_aggr_gara, image, outname
  compile_opt idl2, logical_predicate
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return

  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  mi = envi_get_map_info(fid = fid)
  
  openw, unit, outname, /get_lun
  
  ix = lindgen(nb / 23) * 23
  
  for l = 0, nl -1 do begin
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)

    data = reform(data, ns, 23, nb / 23, /overwrite)
    
    aggr = mean(data, dim = 3)
    
    writeu, unit, aggr
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; 1 == BIL
          , nb = 23, nl = nl, ns = ns $
          , bnames = bnames $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file
end