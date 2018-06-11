pro nrs_zonal_ranking_handle_input, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_refstack')
  widget_control, val_fld, get_value = stackname

  stack_str = strtrim(stackname)
  if strlen(stack_str) eq 0 then return

  outname = getoutname(stack_str, postfix = '_rank', ext = '.dat')

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_outputFile')
  widget_control, val_fld, set_value = outname
end

pro nrs_zonal_ranking_handleok, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_classfile')
  widget_control, val_fld, get_value = classfile

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_step')
  widget_control, val_fld, get_value = step
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_ignore')
  widget_control, val_fld, get_value = ignore

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_ranking_outputFile')
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
    , ysize = 15, title = "Zonal ranking" $
    , /fast_loop $
    )

  nrs_zonal_ranking_temporal, ref, classfile, outname = outname $
    , ignore_value = ignore $
    , step = step $
    , prog_obj = progressBar, cancelled = cancelled

  if obj_valid(progressBar) then $
    progressBar -> Destroy
end
