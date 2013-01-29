pro nrs_bias_removal_handle_target, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_bias_removal_targetstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  energy_name = getOutname(target_str, postfix = '_bias', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_bias_removal_outputFile')
  widget_control, val_fld, set_value = energy_name
end

pro nrs_bias_removal_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_bias_removal_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_bias_removal_targetstack')
  widget_control, val_fld, get_value = target

  val_fld = widget_info(event.top, find_by_uname = 'nrs_bias_removal_outputFile')
  widget_control, val_fld, get_value = outname
  
  if strlen(strtrim(ref, 2)) eq 0 then begin
    void = error_message('Input reference stack not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(strtrim(target, 2)) eq 0 then begin
    void = error_message('Input target stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  envi_open_file, target, r_fid = fid_target, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input reference stack could not be opened!', traceback = 0, /error)
    return
  endif
  
  if fid_target eq -1 then begin
    void = error_message('Input target stack could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Removing bias" $
                        , /fast_loop $
                        )
  
  nrs_bias_removal, fid_ref, fid_target, outname = outname, prog_obj = progressBar, cancelled = cancelled
  
  progressBar -> Destroy
end

