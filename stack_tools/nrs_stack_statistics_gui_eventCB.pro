pro nrs_stack_statistics_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  energy_name = getOutname(target_str, postfix = '_stat', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_outputFile')
  widget_control, val_fld, set_value = energy_name
end

pro nrs_stack_statistics_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_outputFile')
  widget_control, val_fld, get_value = outname
  
  if strlen(strtrim(ref, 2)) eq 0 then begin
    void = error_message('Input reference stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input reference stack could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate stack statistics" $
                        , /fast_loop $
                        )
  
  nrs_stack_statistics, fid_ref, outname = outname, prog_obj = progressBar, cancelled = cancelled
  
  progressBar -> Destroy
end

