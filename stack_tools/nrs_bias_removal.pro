;+
; :Description:
;    Remove bias from image stack. The bias is calculated as the aggregated mean of a second stack.
;    The output is written to file
;
; :Params:
;    fid_base : in, required
;      The file handle of the image stack to calculate the bias from as the mean 
;    fid_pred : in, required
;      The file handle of the image stack to apply the bias to
;
; :Keywords:
;    outname : in/out, optional
;      If specified this will be the name of the output stack. If not specified
;      the automatically assigned name of the output stack is returned 
;    prog_obj : in, optional
;      A progress indicator for visual feedback of the progress
;    cancelled : out, optional
;      If set on return, then the process was aborted by the user
;
; :Author: nieuwenhuis
;-
pro nrs_bias_removal, fid_base, fid_pred, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  if fid_base eq -1 then return
  if fid_pred eq -1 then return

  do_progress = n_elements(prog_obj) gt 0
  if do_progress eq 1 then begin
    prog_obj->setProperty, title = 'Removing bias'
    prog_obj->Start
  endif  
  cancelled = 0
  
  envi_file_query, fid_base, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname
  envi_file_query, fid_pred, bnames = bnames
  mi = envi_get_map_info(fid = fid_base, undefined = undef)
  
  base = make_array(ns, nl, type = dt)
  pred = make_array(ns, nl, type = dt)
  out = make_array(ns, nl, nb, type = dt)
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
    base += envi_get_data(fid = fid_base, dims = dims, pos = b)
    out[*, *, b] = envi_get_data(fid = fid_pred, dims = dims, pos = b)
    pred += out[*, *, b]
  endfor
  bias = (pred - base) / nb
  
  for b = 0, nb - 1 do begin
    out[*, *, b] -= bias
  endfor
  
  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = '_bias', ext = '.')
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames
end
