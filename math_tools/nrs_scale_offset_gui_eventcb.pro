pro nrs_scale_offset_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_input_image')
  widget_control, val_fld, get_value = input
  
  input = strtrim(input, 2)
  if strlen(input) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_outputFile')
  widget_control, val_fld, get_value = outname
  if strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(input, ext = '.', postfix = '_offsca')
    widget_control, val_fld, set_value = outname
  endif
end

pro nrs_scale_offset_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_offset_before_scale_button')
  offset_before_scale = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_input_image')
  widget_control, val_fld, get_value = input
  input = strtrim(input, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_scale')
  widget_control, val_fld, get_value = scale_str
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_offset')
  widget_control, val_fld, get_value = offset_str
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_scale_offset_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  if strlen(input) eq 0 then begin
    void = error_message('Input series not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(outname) eq 0 then begin
    void = error_message('Output name not specified!', traceback = 0, /error)
    return
  endif
  
  scale = float(scale_str[0])
  offset = float(offset_str[0])
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Apply scale and offset" $
                        , /fast_loop $
                        )
  
  nrs_scale_offset, input, outname = outname $
                    , scale = scale, offset = offset, off_before_scale = offset_before_scale $
                    , prog_obj = prog_obj, cancelled = cancelled

  progressBar -> Destroy
end

