pro nrs_ll_grid_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_input_image')
  widget_control, val_fld, get_value = input
  
  input = strtrim(input, 2)
  if strlen(input) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_x_outputFile')
  widget_control, val_fld, get_value = outname
  if strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(input, ext = '.', postfix = '_lon')
    widget_control, val_fld, set_value = outname
  endif
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_y_outputFile')
  widget_control, val_fld, get_value = outname
  if strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(input, ext = '.', postfix = '_lat')
    widget_control, val_fld, set_value = outname
  endif
  
  envi_open_file, input, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  projection = envi_get_projection(fid = fid)
  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_latlon_button')
  widget_control, val_fld, sensitiv = projection.type gt 1
  
end

pro nrs_ll_grid_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_latlon_button')
  force_geographic = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_input_image')
  widget_control, val_fld, get_value = input
  input = strtrim(input, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_x_outputFile')
  widget_control, val_fld, get_value = outname_x
  outname_x = strtrim(outname_x, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_ll_grid_y_outputFile')
  widget_control, val_fld, get_value = outname_y
  outname_y = strtrim(outname_y, 2)
  
  if strlen(input) eq 0 then begin
    void = error_message('Input image not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(outname_x) eq 0 and strlen(outname_y) eq 0 then begin
    void = error_message('At least one output needs to be specified!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Normalize series" $
                        , /fast_loop $
                        )
  
  nrs_generate_LL_grid, input, force_geographic = force_geographic $
                  , x_filename = outname_x, y_filename = outname_y $ 
                  , prog_obj = progressBar, cancelled = cancelled

  progressBar -> Destroy
end

