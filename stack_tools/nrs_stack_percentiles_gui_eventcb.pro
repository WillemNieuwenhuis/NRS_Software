pro nrs_stack_percentiles_image_change, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_image')
  widget_control, val_fld, get_value = image
  image = strtrim(image[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_outputFile')

  if (n_elements(image) gt 0) && (strlen(image) gt 0) then begin
    outname = getoutname(image, postfix = '_perc', ext = '.dat')
    widget_control, val_fld, set_value = outname
  endif
  
end

pro nrs_stack_percentiles_exec, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_image')
  widget_control, val_fld, get_value = image
  image = strtrim(image[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_percentiles')
  widget_control, val_fld, get_value = percentile  ; percentiles as comma-separated string
  percentile = strtrim(percentile[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_ignore')
  widget_control, val_fld, get_value = ignore_value

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname[0], 2)

;  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_percentiles_exact_button')
;  exact = widget_info(val_fld, /button_set)
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate stack percentiles" $
                        , /fast_loop $
                        )
  
  nrs_stack_percentiles, image, outname = outname $
                       , percentile = percentile $
                       , ignore_value = ignore_value[0] $
                       , prog_obj = prog_obj, cancelled = cancelled, exact = exact
  
;  image, outname = outname, exact = exact $
;    , percentile = percentile $
;    , ignore_value = ignore_value[0] $
;    , cancelled = cancelled, prog_obj = prog_obj
  
  if prog_obj ne !null then $
    prog_obj -> Destroy

end
