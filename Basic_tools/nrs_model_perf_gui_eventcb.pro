pro nrs_model_perf_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_obs')
  widget_control, val_fld, get_value = obs

  obs = strtrim(obs, 2)
  if strlen(obs) eq 0 then return

  envi_open_file, obs, r_fid = fid, /no_realize, /no_interactive_query
  envi_file_query, fid, nb = nb
  
  ext = nb gt 1 ? '.' : '.txt'
  basename = getOutname(obs, postfix = '_mper', ext = ext)
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_outputFile')
  widget_control, val_fld, set_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_num_bands')
  widget_control, val_fld, set_value = string(nb, format = '(i0)')
end

pro nrs_model_perf_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_obs')
  widget_control, val_fld, get_value = obs
  obs = strtrim(obs, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_pred')
  widget_control, val_fld, get_value = prd
  prd = strtrim(prd, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_rmse_button')
  doRMSE = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_ef_button')
  doEF = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_cd_button')
  doCD = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_e_button')
  doE = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_mre_button')
  doMRE = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_model_perf_md_button')
  doMD = widget_info(val_fld, /button_set)
  
  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate model performance" $
                        , /fast_loop $
                        )
  
  nrs_model_performance, obs, prd, outname = basename $
                       , rmse = doRMSE, ef = doEF, cd = doCD, re = doE, mre = doMRE, md = doMD $ 
                       , prog_obj = progressBar, cancelled = cancelled
  
  if progressBar ne !null then $
    progressBar -> Destroy
end

