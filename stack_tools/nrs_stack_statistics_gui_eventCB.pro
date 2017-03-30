pro nrs_stack_statistics_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  energy_name = getOutname(target_str, postfix = '_stat', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_outputFile')
  widget_control, val_fld, set_value = energy_name
end

pro nrs_stack_statistics_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_ignore')
  widget_control, val_fld, get_value = ignore_value

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_statistics_outputFile')
  widget_control, val_fld, get_value = outname

  ref = strtrim(ref, 2)
  if strlen(ref) eq 0 then begin
    void = error_message('Input reference stack not specified!', traceback = 0, /error)
    return
  endif

  ignore_value = ignore_value[0]
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate stack statistics" $
                        , /fast_loop $
                        )
  
  nrs_stack_statistics, ref, outname = outname, ignore_value = ignore_value, /ignore_undef $
                      , prog_obj = progressBar, cancelled = cancelled
  
  if obj_valid(progressBar) then progressBar->Destroy
end

