pro nrs_rpd_perc_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_locations')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_gwi', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rpd_perc_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_rpd_perc_handleOK, event
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
  
;    ; initialise tranquilizer
;  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
;                        , ysize = 15, title = "(Grouped) aggregation" $
;                        , /fast_loop $
;                        )

  nrs_rpd_at_location, ref, table, out_name = out_name, perc = perc $
                     , prog_obj = prog_obj, cancelled = cancelled

  if prog_obj ne !null then $
    prog_obj -> Destroy
end

