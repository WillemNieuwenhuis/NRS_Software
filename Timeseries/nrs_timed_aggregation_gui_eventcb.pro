pro nrs_timed_aggregation_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_taggr', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_outputFile')
  widget_control, val_fld, set_value = basename
  
  envi_open_file, target_str, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  envi_file_query, fid, nb = nb
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_start_date')
  widget_control, val_fld, get_value = str_sd

  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_end_date')
  widget_control, val_fld, get_value = str_ed
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_period_aggr_input_period_label')
  if strlen(strtrim(str_sd, 2)) eq 0 || strlen(strtrim(str_ed, 2)) eq 0 then return
  sd = nrs_str2julian(str_sd)
  ed = nrs_str2julian(str_ed)
  if ed lt sd then begin
    widget_control, val_fld, set_value = ''
    return
  endif

  per = nrs_get_period_from_range(sd, ed, nb, per_str = input_period)

  widget_control, val_fld, set_value = 'Input period: ' + input_period
end

pro nrs_timed_aggregation_toggle_levels, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_lvl1_button')
  isOn1 = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_lvl2_button')
  isOn2 = widget_info(val_fld, /button_set)
  widget_control, val_fld, sensitiv = isOn1
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_level1_combo')
  widget_control, val_fld, sensitiv = isOn1
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_level2_combo')
  widget_control, val_fld, sensitiv = (isOn1 && isOn2)
end

pro nrs_timed_aggregation_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_start_date')
  widget_control, val_fld, get_value = start_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_end_date')
  widget_control, val_fld, get_value = end_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_aggr_combo')
  aggr_interval = widget_info(val_fld, /combobox_gettext)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_indices_combo')
  aggr_func = widget_info(val_fld, /combobox_gettext)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_lvl1_button')
  isOn1 = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_lvl2_button')
  isOn2 = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_level1_combo')
  isOn1 = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_timed_aggregation_level2_combo')
  isOn2 = widget_info(val_fld, /button_set)

  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Timed aggregation" $
                        , /fast_loop $
                        )
  
  nrs_timed_aggregation, ref $
                   , start_date, end_date $
                   , aggr_interval, aggr_func $
                   , outname = outname $
                   , prog_obj = progressBar, cancelled = cancelled

  if progressBar ne !null then $
    progressBar -> Destroy
end

