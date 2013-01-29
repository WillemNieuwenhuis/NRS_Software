pro nrs_drought_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_drought_VCI_image')
  widget_control, val_fld, get_value = input
  
  input = strtrim(input, 2)
  if strlen(input) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_drought_outputFile')
  widget_control, val_fld, get_value = outname
  if strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(input, ext = '.', postfix = '_vtci')
    widget_control, val_fld, set_value = outname
  endif
end

pro nrs_drought_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_drought_VCI_image')
  widget_control, val_fld, get_value = vci_input
  vci_input = strtrim(vci_input, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_drought_TCI_image')
  widget_control, val_fld, get_value = tci_input
  tci_input = strtrim(tci_input, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_drought_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  if strlen(vci_input) eq 0 then begin
    void = error_message('Input VCI image not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(tci_input) eq 0 then begin
    void = error_message('Input TCI image not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(outname) eq 0 then begin
    void = error_message('Output name not specified!', traceback = 0, /error)
    return
  endif
  
  nrs_drought_index, vci_input, tci_input, outname = outname
end

