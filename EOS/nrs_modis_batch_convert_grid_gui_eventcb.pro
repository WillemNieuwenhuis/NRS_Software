pro nrs_modis_batch_convert_grid_handleok, event
  compile_opt idl2, logical_predicate

  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_modis_batch_convert_grid_infolder')
  widget_control, fld, get_value = infolder

  fld = widget_info(event.top, find_by_uname = 'nrs_modis_batch_convert_grid_outfolder')
  widget_control, fld, get_value = outfolder


  ; check input values
  infolder = strtrim(infolder[0], 2)
  outfolder = strtrim(outfolder[0], 2)

  if strlen(infolder) eq 0 then begin
    void = dialog_message('Please specify an input folder', /error)
    return
  endif

  if strlen(outfolder) eq 0 then begin
    void = dialog_message('Please specify an output folder', /error)
    return
  endif

  nrs_modis_batch_convert_grid, infolder, outfolder, /progress

end