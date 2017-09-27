pro nrs_convert_modis_grid_handle_input, event
  compile_opt idl2

  fld = widget_info(event.top, find_by_uname = 'nrs_modis_convert_grid_infile')
  widget_control, fld, get_value = infile

  infile = strtrim(infile[0], 2)
  if strlen(infile) eq 0 then return

  fld = widget_info(event.top, find_by_uname = 'nrs_modis_convert_grid_outfile')
  widget_control, fld, get_value = outfile

  outfile = strtrim(outfile[0], 2)
  if strlen(outfile) gt 0 then return

  outfile = getoutname(infile, postfix = '_cnv', ext = '.dat')

  widget_control, fld, set_value = outfile

end

pro nrs_modis_convert_grid_handleok, event
  compile_opt idl2, logical_predicate

  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_modis_convert_grid_infile')
  widget_control, fld, get_value = infile

  fld = widget_info(event.top, find_by_uname = 'nrs_modis_convert_grid_outfile')
  widget_control, fld, get_value = outfile


  ; check input values
  infile = strtrim(infile[0], 2)
  outfile = strtrim(outfile[0], 2)

  if strlen(infile) eq 0 then begin
    void = dialog_message('Please specify an input MODIS Gris product', /error)
    return
  endif

  if strlen(outfile) eq 0 then begin
    void = dialog_message('Please specify an output filename', /error)
    return
  endif

  nrs_modis_convert_grid, infile, outfile, close_file = 0

end