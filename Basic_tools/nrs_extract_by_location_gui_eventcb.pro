pro nrs_extract_by_location_handle_points, event
  compile_opt idl2, logical_predicate
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_by_location_points')
  widget_control, fld, get_value = in_table

  in_table = strtrim(in_table, 2)
  if strlen(in_table) eq 0 then return

  outname = getoutname(in_table, postfix = '_ebl', ext = '.shp')

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_by_location_outputFile')
  widget_control, fld, set_value = outname
  
end

pro nrs_extract_by_location_handleok, event
  compile_opt idl2, logical_predicate

  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_by_location_points')
  widget_control, fld, get_value = shapefile

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_by_location_refstack')
  widget_control, fld, get_value = infile

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_by_location_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  shapefile = strtrim(shapefile, 2)
  infile = strtrim(infile, 2)
  outfile = strtrim(outfile, 2)

  if strlen(shapefile) eq 0 then begin
    void = dialog_message('No input table specified', /error)
    return
  endif
  if strlen(infile) eq 0 then begin
    void = dialog_message('No input image specified', /error)
    return
  endif

  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif

  ; start extraction
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Extract value by location' $
    , /fast_loop $
    )

  nrs_extract_by_location, shapefile, infile, outshape = outfile $
    , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $    
    prog_obj->destroy

end
