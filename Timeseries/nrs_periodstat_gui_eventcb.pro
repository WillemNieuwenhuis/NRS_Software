pro nrs_periodstat_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_gaggr', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_periodstat_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_img_py')
  widget_control, val_fld, get_value = str_py

  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_mean_button')
  doMean = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_min_button')
  doMin = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_max_button')
  doMax = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_stddev_button')
  doStddev = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_periodstat_median_button')
  doMedian = widget_info(val_fld, /button_set)
  
  img_py = fix(str_py)
  
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
                        , ysize = 15, title = "(Grouped) aggregation" $
                        , /fast_loop $
                        )
  
  nrs_grouped_aggregation, ref $
                   , start_date, end_date $
                   , aggr_interval, aggr_func $
                   , grouped = grouped $
                   , outname = outname $
                   , prog_obj = prog_obj, cancelled = cancelled

  if progressBar ne !null then $
    progressBar -> Destroy
end

