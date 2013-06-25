pro nrs_water_index_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_combo')
  water_index = widget_info(val_fld, /combobox_gettext)

  basename = getOutname(target_str, postfix = '_' + water_index, ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_water_index_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_water_index_combo')
  water_index = widget_info(val_fld, /combobox_gettext)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input image not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  if fid_ref eq -1 then begin
    void = error_message('Could not open input image!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Water index' $
                        , /fast_loop $
                        )
  
  nrs_water_index, ref $
                   , water_index $
                   , outname = outname $
                   , prog_obj = progressBar, cancelled = cancelled

  if progressBar ne !null then $
    progressBar -> Destroy
end

