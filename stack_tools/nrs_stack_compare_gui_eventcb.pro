pro nrs_stack_compare_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_source_stack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_scmp', ext = '.txt')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_report_file')
  widget_control, val_fld, set_value = basename
end

pro nrs_stack_compare_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_source_stack')
  widget_control, val_fld, get_value = src
  source = strtrim(src, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_target_stack')
  widget_control, val_fld, get_value = trg
  target = strtrim(trg, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_report_file')
  widget_control, val_fld, get_value = rep
  report_file = strtrim(rep, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_bname_button')
  do_bnames = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_compare_bvalue_button')
  do_values = widget_info(val_fld, /button_set)
  
  if strlen(source) eq 0 then begin
    void = error_message('Source stack not specified!', traceback = 0, /error)
    return
  endif
  envi_open_file, source, r_fid = fid_src, /no_realize, /no_interactive_query
  if fid_src eq -1 then begin
    void = error_message('Source stack could not be opened!', traceback = 0, /error)
    return
  endif
  
  if strlen(target) eq 0 then begin
    void = error_message('Target stack not specified!', traceback = 0, /error)
    return
  endif
  envi_open_file, target, r_fid = fid_trg, /no_realize, /no_interactive_query
  if fid_trg eq -1 then begin
    void = error_message('Target stack could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Compare stacks" $
                        , /fast_loop $
                        )
  
  nrs_compare_stacks, source, target, show_bnames = do_bnames, show_bands = do_values $
                    , report_file = report_file $
                    , prog_obj = progressBar, cancelled = cancelled
  
  progressBar -> Destroy
end

