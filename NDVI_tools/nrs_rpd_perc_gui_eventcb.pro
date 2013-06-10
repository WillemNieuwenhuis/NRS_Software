pro nrs_rpd_perc_handle_input, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_locations')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_gwi', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_rpd_perc_toggle_window, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_window_button')
  isOn = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_wbase')
  widget_control, val_fld, sensitiv = isOn

end

pro nrs_rpd_perc_handleOK, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_locations')
  widget_control, val_fld, get_value = table
  table = strtrim(table, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_percentage')
  widget_control, val_fld, get_value = perc
  perc = fix(perc[0])

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_outputFile')
  widget_control, val_fld, get_value = out_name

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_window_button')
  use_window = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_valid_button')
  winter_corr = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_window')
  widget_control, val_fld, get_value = win_side

  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(table) eq 0 then begin
    void = error_message('Input locations table not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif

  if use_window then begin
    win_side = fix(win_side[0])
    if (win_side mod 2) eq 0 then win_side++
    if win_side lt 3 then win_side = 0
  endif else win_side = 0
  
;    ; initialise tranquilizer
;  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
;                        , ysize = 15, title = "(Grouped) aggregation" $
;                        , /fast_loop $
;                        )

  nrs_rpd_at_location, ref, table, out_name = out_name, perc = perc $
                     , window = win_side $
                     , winter_corr = winter_corr $
                     , prog_obj = prog_obj, cancelled = cancelled

  if prog_obj ne !null then $
    prog_obj -> Destroy
end

