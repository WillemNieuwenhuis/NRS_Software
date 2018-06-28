pro nrs_zonal_threshold_handle_input, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_refstack')
  widget_control, val_fld, get_value = stackname

  stack_str = strtrim(stackname)
  if strlen(stack_str) eq 0 then return

  outname = getoutname(stack_str, postfix = '_thr', ext = '.dat')

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_outputFile')
  widget_control, val_fld, set_value = outname
end

pro nrs_zonal_threshold_handleok, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_classfile')
  widget_control, val_fld, get_value = classfile

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_threshold')
  widget_control, val_fld, get_value = threshold
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_ignore')
  widget_control, val_fld, get_value = ignore

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_threshold_outputFile')
  widget_control, val_fld, get_value = outname

  if strlen(strtrim(ref, 2)) eq 0 then begin
    void = error_message('Input stack not specified!', traceback = 0, /error)
    return
  endif

  if strlen(strtrim(classfile, 2)) eq 0 then begin
    void = error_message('Zonal image not specified!', traceback = 0, /error)
    return
  endif

  ; initialise tranquilizer
  progressBar = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Zonal threshold" $
    , /fast_loop $
    )

  nrs_zonal_threshold_spatial, ref, classfile, outname = outname $
    , ignore_value = ignore $
    , threshold = threshold $
    , prog_obj = progressBar, cancelled = cancelled

  if obj_valid(progressBar) then $
    progressBar -> Destroy
end
