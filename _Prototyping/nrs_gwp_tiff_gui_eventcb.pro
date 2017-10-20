pro nrs_gwp_tiff_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_gwp_tiff_file')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target[0])
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_gwp', ext = '.tif')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_gwp_tiff_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_gwp_tiff_handleok, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_gwp_tiff_file')
  widget_control, val_fld, get_value = input

  input_str = strtrim(input[0])
  if strlen(input_str) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_gwp_tiff_outputFile')
  widget_control, val_fld, get_value = output

  output_str = strtrim(output[0])
  if strlen(output_str) eq 0 then return
  
  nrs_gwp_convert, input_str, output_str
  
end
