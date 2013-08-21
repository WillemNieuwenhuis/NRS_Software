;+
; :Description:
;    Determine model performance. The output contains:
;    <ol>
;    <li>RMSE
;    <li>EF (model efficiency)
;    <li>CD (coefficient of determination)
;    <li>E (relative error)
;    <li>MRE (mean relative error)
;    <li>MD (mean difference)
;    </ol>
;
; :Params:
;    observed : in
;      Observed / measured values
;    predicted : in
;      Model predicted values
;
; :Keywords:
;    outname : in
;      Name of the output
;    rmse : in
;      If set calculate RMSE
;    ef : in
;      If set, calculate EF
;    cd : in
;      If set, calculate CD
;    re :
;      If set, calculate E
;    mre :
;      If set calculate MRE
;    md : in
;      If set, calculate MD
;    all : in
;      If set, calculate all coefficients 
;    prog_obj : in
;      Progress indicator object
;    cancelled : out
;      Indicate failure or user break
;
; :Author: nieuwenhuis
;-
pro nrs_model_performance, observed, predicted, outname = outname $
                         , rmse = rmse, ef = ef, cd = cd, re = re, mre = mre, md = md $
                         , all = all $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, observed, r_fid = fid_obs, /no_realize, /no_interactive_query
  if fid_obs eq -1 then return

  envi_open_file, predicted, r_fid = fid_prd, /no_realize, /no_interactive_query
  if fid_prd eq -1 then return
  
  envi_file_query, fid_obs, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid_obs, undefined = csy_undef)
  if csy_undef then delvar, mi
  
  envi_file_query, fid_prd, ns = nsp, nl = nlp, nb = nbp, dims = dimsp, data_type = dtp, data_ignore_value = undefp
  mip = envi_get_map_info(fid = fid_prd, undefined = csy_undefp)
  if csy_undefp then delvar, mip
  
  if total(dims eq dimsp) ne 5 then return
  
  doSpectral = nb gt 1
  
  cancelled = 0
  
  doAll  = ~(n_elements(all)  eq 0 || all eq 0)
  doRMSE = ~(n_elements(rmse) eq 0 || rmse eq 0)
  doEF   = ~(n_elements(ef)   eq 0 || ef eq 0)
  doCD   = ~(n_elements(cd)   eq 0 || cd eq 0)
  doRE   = ~(n_elements(re)   eq 0 || re eq 0)
  doMRE  = ~(n_elements(mre)  eq 0 || mre eq 0)
  doMD   = ~(n_elements(md)   eq 0 || md eq 0)

  do_arr = [doRMSE, doEF, doCD, doRE, doMRE, doMD]
  if doAll then do_arr = intarr(n_elements(do_arr)) + 1
  do_ix = where(do_arr eq 1, cix)
  if cix eq 0 then begin
    ans = dialog_message('Nothing to do, quitting', title = 'Information', /information)
    return
  endif
  do_rix = fix(total(do_arr, /cum)) - 1
  
  ext = doSpectral ? '.dat' :'.txt'
  if n_elements(outname) eq 0 then outname = getOutname(observed, postfix = '_mper', ext = ext)
  if doSpectral then begin
    ; per pixel
    nrs_set_progress_property, prog_obj, /start, title = 'Calculating model performance'
    
    nr_coefs = total(do_arr)
    outdata = fltarr(ns, nl, nr_coefs)
    bnames_all = ['MRSE','R2/EF','CD','E','MRE','MD']
    bnames = bnames_all[do_ix]
    pix = lindgen(ns, nl)
    qix = pix mod ns
    mn_mat = fltarr(ns, nb, /nozero)
    for l = 0, nl - 1 do begin
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
      
      n = nb
      obs = envi_get_slice(fid = fid_obs, line = l, xs = 0, xe = ns - 1)
      prd = envi_get_slice(fid = fid_prd, line = l, xs = 0, xe = ns - 1)
      mean_obs = mean(obs, dim = 2)
      mn_mat[pix] = mean_obs[qix]
      sum_sqr = total((obs - prd) ^ 2, 2)
      sum_sqr_obs = total((obs - mn_mat) ^ 2, 2)
      sum_sqr_prd = total((prd - mn_mat) ^ 2, 2)
      
      if do_arr[0] then outdata[*, l, do_rix[0]] = sqrt(sum_sqr / n)              ; RMSE
      if do_arr[1] then outdata[*, l, do_rix[1]] = 1.0 - sum_sqr / sum_sqr_obs    ; EF (model efficiency)
      if do_arr[2] then outdata[*, l, do_rix[2]] = sum_sqr_obs / sum_sqr_prd      ; CD (coefficient of determination)
      if do_arr[3] then outdata[*, l, do_rix[3]] = total((obs - prd) / obs, 2) * 100.0 / n ; E (relative error)
      if do_arr[4] then outdata[*, l, do_rix[4]] = total(abs((obs - prd) / obs), 2) * 100.0 / n  ; MRE (mean relative error)
      if do_arr[5] then outdata[*, l, do_rix[5]] = total(prd - obs, 2) / n           ; MD (mean difference)
    endfor
    
    envi_write_envi_file, outdata, out_name = outname, map_info = mi, bnames = bnames
  endif else begin
    ; per image
    n = ns * nl
    
    obs = envi_get_data(fid = fid_obs, dims = dims, pos = 0)
    prd = envi_get_data(fid = fid_prd, dims = dims, pos = 0)
    
    mean_obs = mean(obs)
    sum_sqr = total((obs - prd) ^ 2)
    sum_sqr_obs = total((obs - mean_obs) ^ 2)
    sum_sqr_prd = total((prd - mean_obs) ^ 2)
    
    openw, lun, outname, /get_lun
    printf, lun, 'Observed:  ' + observed
    printf, lun, 'Predicted: ' + predicted
    if do_arr[0] then printf, lun, 'RMSE:  ' + string(sqrt(sum_sqr / n), format = '(g0.6)')
    if do_arr[1] then printf, lun, 'R2/EF: ' + string(1.0 - sum_sqr / sum_sqr_obs, format = '(g0.6)')
    if do_arr[2] then printf, lun, 'CD:    ' + string(sum_sqr_obs / sum_sqr_prd, format = '(g0.6)')
    if do_arr[3] then printf, lun, 'E:     ' + string(total((obs - prd) / obs) * 100.0 / n, format = '(g0.6)')
    if do_arr[4] then printf, lun, 'MRE:   ' + string(total(abs((obs - prd) / obs)) * 100.0 / n, format = '(g0.6)')
    if do_arr[5] then printf, lun, 'MD:    ' + string(total(prd - obs) / n, format = '(g0.6)')
    free_lun, lun
  endelse
  
end