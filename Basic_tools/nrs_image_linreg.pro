pro nrs_image_linreg, image, output = coef, rmse = rmse $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  ; check params
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then mi = temporary(dummy) 
  
  do_rmse = keyword_set(rmse)
  
  if n_elements(coef) eq 0 then coef_file = getOutname(image, postfix = '_coef', ext = '.dat')
  
  cancelled = 0

  nrs_set_progress_property, prog_obj, /start, title = 'Calculating linear regression'
  
  pos = indgen(nb)
  nbcoef = do_rmse ? 3 : 2
  cdata = fltarr(ns, nl, nbcoef)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled, /console) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    for col = 0, ns - 1 do begin
      reg = linfit(pos, slice[col, *], chi = chi)
      if do_rmse then reg = [reg, sqrt(chi / nb)] 
      cdata[col, line, *] = reg
    endfor
  endfor
  
  bnames = ['a', 'b']
  if do_rmse then bnames = [bnames, 'rmse']
  envi_write_envi_file, cdata, out_name = coef_file, map_info = mi, bnames = bnames
  
  
end