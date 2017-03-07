pro nrs_stack_quartiles_image_change, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_quartiles_image')
  widget_control, val_fld, get_value = image
  image = strtrim(image[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_quartiles_outputFile')

  if (n_elements(image) gt 0) && (strlen(image) gt 0) then begin
    outname = getoutname(image, postfix = '_quar', ext = '.dat')
    widget_control, val_fld, set_value = outname
  endif
  
end

pro nrs_stack_quartiles_exec, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_quartiles_image')
  widget_control, val_fld, get_value = image
  image = strtrim(image[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_quartiles_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_quartiles_exact_button')
  exact = widget_info(val_fld, /button_set)
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate stack quartiles" $
                        , /fast_loop $
                        )
  
  nrs_stack_quartiles, image, outname = outname, prog_obj = prog_obj, cancelled = cancelled, exact = exact
  
  if prog_obj ne !null then $
    prog_obj -> Destroy

end
