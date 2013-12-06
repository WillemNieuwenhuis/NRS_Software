pro nrs_growing_degree_days_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target, 2)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_gdd', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_outputFile')
  widget_control, val_fld, get_value = outfile
  outfile_str = strtrim(outfile, 2)
  if strlen(outfile_str) eq 0 then $
    widget_control, val_fld, set_value = basename
end

pro nrs_growing_degree_days_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_jerk_button')
  calc_jerk = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_growing_degree_days_spring_button')
  calc_spring_start = widget_info(val_fld, /button_set)

  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif

  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif
  
  envi_file_query, fid_ref, nb = nb
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Growing degree days" $
                        , /fast_loop $
                        )
  
  nrs_growing_degree_days, ref $
                       , calc_jerk = calc_jerk $
                       , calc_spring_start = calc_spring_start $
                       , outname = outname $
                       , prog_obj = prog_obj, cancelled = cancelled

  if prog_obj ne !null then $
    prog_obj -> Destroy
end

