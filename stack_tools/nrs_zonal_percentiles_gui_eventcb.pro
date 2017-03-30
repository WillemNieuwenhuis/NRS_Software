pro nrs_zonal_percentiles_group_button_toggle, event
  compile_opt idl2
  
  if (event.id eq widget_info(event.top, find_by_uname='nrs_zonal_percentiles_group_button')) then begin
    isOn = widget_info(event.id, /button_set)
    cpanel = widget_info(event.top, find_by_uname='nrs_zonal_percentiles_imgperyear')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro nrs_zonal_percentiles_handle_input, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_refstack')
  widget_control, val_fld, get_value = stackname
  
  stack_str = strtrim(stackname)
  if strlen(stack_str) eq 0 then return
  
  outname = getoutname(stack_str, postfix = '_pctl', ext = '.csv')
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_outputFile')
  widget_control, val_fld, set_value = outname
end

pro nrs_zonal_percentiles_handleok, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_refstack')
  widget_control, val_fld, get_value = ref
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_classfile')
  widget_control, val_fld, get_value = classfile
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_percentiles')
  widget_control, val_fld, get_value = percentiles
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_ignore')
  widget_control, val_fld, get_value = ignore
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_raster_button')
  create_raster = widget_info(val_fld, /button_set)

  valfld = widget_info(event.top, find_by_uname = 'nrs_zonal_percentiles_group_button')
  doGrouping = widget_info(valfld, /button_set)
  if doGrouping then begin
    valfld = widget_info(event.top, find_by_uname='nrs_zonal_percentiles_imgperyear')
    widget_control, valfld, get_value = imgpy
    img_per_period = fix(strtrim(imgpy, 2))
  endif

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
    , ysize = 15, title = "Zonal percentiles" $
    , /fast_loop $
    )

  if doGrouping then begin    
    nrs_zonal_percentiles_group, ref, classfile, outname = outname $
      , ignore_value = ignore $
      , percentile = percentiles $
      , img_per_period = img_per_period[0] $
      , create_raster = create_raster $
      , prog_obj = progressBar, cancelled = cancelled
  endif else begin
    nrs_zonal_percentiles, ref, classfile, outname = outname $
      , ignore_value = ignore $
      , percentile = percentiles $
      , create_raster = create_raster $
      , prog_obj = progressBar, cancelled = cancelled
  endelse
  
  if obj_valid(progressBar) then $
    progressBar -> Destroy
end
