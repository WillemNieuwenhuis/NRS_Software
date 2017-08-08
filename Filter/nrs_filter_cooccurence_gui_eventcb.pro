pro nrs_filter_cooccurence_handle_points, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_points')
  widget_control, fld, get_value = in_table

  in_table = strtrim(in_table, 2)
  if strlen(in_table) eq 0 then return

  outname = getoutname(in_table, postfix = '_cooc', ext = '.shp')

  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_outputFile')
  widget_control, fld, set_value = outname

end

pro nrs_filter_cooccurence_handleok, event
  compile_opt idl2, logical_predicate

  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_points')
  widget_control, fld, get_value = shapefile

  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_refstack')
  widget_control, fld, get_value = infile

  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_kernel')
  widget_control, fld, get_value = kernel

  fld = widget_info(event.top, find_by_uname = 'nrs_filter_cooccurence_outputFile')
  widget_control, fld, get_value = outfile


  ; check input values
  shapefile = strtrim(shapefile, 2)
  infile = strtrim(infile, 2)
  outfile = strtrim(outfile, 2)

  if strlen(shapefile) eq 0 then begin
    void = dialog_message('No polygon shapefile specified', /error)
    return
  endif
  if strlen(infile) eq 0 then begin
    void = dialog_message('No input image specified', /error)
    return
  endif
  
  kernel = fix(kernel[0])

  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif

  ; start extraction
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Texture co-occurrence' $
    , /fast_loop $
    )

  nrs_filter_cooccurence, infile, shapefile, outshape = outfile $
    , kernel = kernel $
    , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $
    prog_obj->destroy

end
