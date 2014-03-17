pro nrs_stack_reverse_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_refstack')
  widget_control, val_fld, get_value = target
  
  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return
  
  energy_name = getoutname(target_str, postfix = '_rev', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_outputFile')
  widget_control, val_fld, set_value = energy_name
end

pro nrs_stack_reverse_handleok, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_refstack')
  widget_control, val_fld, get_value = ref
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_bname_button')
  keep_bnames = widget_info(val_fld, /button_set)
  
  if strlen(strtrim(ref, 2)) eq 0 then begin
    void = error_message('Input stack not specified!', traceback = 0, /error)
    return
  endif
  
  ; initialise tranquilizer
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Reverse band order' $
    , /fast_loop $
    )
    
  nrs_stack_reverse, ref, outname = outname $
                   , keep_bnames = keep_bnames $
                   , prog_obj = prog_obj, cancelled = cancelled
  
  if obj_valid(prog_obj) then $
    prog_obj -> Destroy
end

