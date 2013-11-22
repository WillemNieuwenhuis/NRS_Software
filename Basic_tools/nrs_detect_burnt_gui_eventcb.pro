pro nrs_detect_burnt_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_obs')
  widget_control, val_fld, get_value = obs

  obs = strtrim(obs, 2)
  if strlen(obs) eq 0 then return

  basename = getOutname(obs, postfix = '_burn', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_detect_burnt_toggle_column, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_use_thresh_column')
  useColumn = widget_info(val_fld, /button_set)

  thr_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_threshold')
  avg_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_avg_button')
  widget_control, thr_fld, sensitive = ~useColumn
  widget_control, avg_fld, sensitive = useColumn
  
end

pro nrs_detect_burnt_handleOK, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_obs')
  widget_control, val_fld, get_value = table
  table = strtrim(table, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_threshold')
  widget_control, val_fld, get_value = fixed_thresh
  fixed_thresh = float(fixed_thresh[0])

  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_filter_noise')
  strict = widget_info(val_fld, /button_set) eq 0
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_use_thresh_column')
  useColumn = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_detect_burnt_avg_button')
  useAvg = widget_info(val_fld, /button_set)
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Detect burnt periods" $
                        , /fast_loop $
                        )
  
  if useColumn then void = temporary(fixed_thresh)  ; undefine fixed_thresh
  
  nrs_detect_burnt, table $
                    , basename = basename $
                    , strict = strict $
                    , fixed_thresh = fixed_thresh $
                    , single_thresh = useAvg $
                    , prog_obj = prog_obj, cancelled = cancelled
  
  ans = dialog_message('Finished!', /information)
  
  if prog_obj ne !null then $
    prog_obj -> Destroy
end

