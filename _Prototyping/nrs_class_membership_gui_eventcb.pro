pro nrs_class_membership_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_image')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target[0])
  if strlen(target_str) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_output')
  widget_control, val_fld, set_value = target_str
end

pro nrs_class_membership_handleok, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_image')
  widget_control, val_fld, get_value = input
  input = strtrim(input[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_kernel')
  widget_control, val_fld, get_value = kernel
  kernel = fix(strtrim(kernel[0], 2))

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_ignore')
  widget_control, val_fld, get_value = nodata
  nodata = strtrim(nodata[0], 2)
  if strlen(nodata) gt 0 then $ 
    nodata = float(nodata) $
  else nodata = temporary(dummy)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_class_membership_output')
  widget_control, val_fld, get_value = outbase
  outbase = strtrim(outbase[0], 2)

  if strlen(input) eq 0 then begin
    void = error_message('Input data not specified!', traceback = 0, /error)
    return
  endif

  ; initialise tranquilizer
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Determine class membership' $
    , /fast_loop $
    )
  prog_obj->Start

  nrs_class_membership, input $
    , outname = outbase $
    , kernel = kernel $
    , ignore_value = nodata $
    , cancelled = cancelled, prog_obj = prog_obj
    
  if obj_valid(prog_obj) then $
    prog_obj->Destroy
end

