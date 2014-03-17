pro nrs_stack_reverse, image, outname = outname $
             , keep_bnames = keep_bnames $
             , prog_obj = prog_obj, cancelled = cancelled

  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims $
    , data_type = dt $
    , bnames = bnames

  keep_bnames = keyword_set(keep_bnames)  
  
  cancelled = 0
  
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_rev', ext = '.dat')
  
  nrs_set_progress_property, prog_obj, /start, title = 'Reverse stack bands'
  
  openw, unit, outname, /get_lun
  ix = nb - indgen(nb) - 1
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then begin
      break
    endif
    
    data = envi_get_data(fid = fid, dims = dims, pos = ix[b])
    
    writeu, unit, data
    
  endfor

  if keep_bnames && (n_elements(bnames) eq nb) then begin
    bnames = bnames[ix]
  endif

  meta = envi_set_inheritance(fid, dims, /full)
  
  close, unit
  free_lun, unit
  envi_setup_head, fname = outname $
    , data_type = dt $
    , /write $
    , interleave = 0 $  ; 0 == BSQ
    , ns = ns, nl= nl, nb = nb $
    , bnames = bnames $
    , inherit = meta
    
end
