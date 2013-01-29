pro nrs_time_interpol_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_tinp', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_time_interpol_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_start_date')
  widget_control, val_fld, get_value = str_sd

  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_end_date')
  widget_control, val_fld, get_value = str_ed
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_time_interpol_period_combo')
  period = widget_info(val_fld, /combobox_gettext)
  period = strtrim(period, 2)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(period) eq 0 then begin
    void = error_message('Output time period not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "(Grouped) aggregation" $
                        , /fast_loop $
                        )
  
  nrs_time_interpolate, ref, outname = basename $
                   , str_sd, str_ed $
                   , period $
                   , prog_obj = prog_obj, cancelled = cancelled

  if prog_obj ne !null then $
    prog_obj -> Destroy
end

