pro nrs_zonal_batch_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_folder')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target[0])
  if strlen(target_str) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_use_table_button')
  use_table = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_outputFolder')
  widget_control, val_fld, set_value = target_str
end

pro nrs_zonal_batch_handle_shapefile, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_shapefile')
  widget_control, val_fld, get_value = shapefile
  shapefile = strtrim(shapefile[0], 2)

  if strlen(shapefile) gt 0 then begin
    fi = file_info(shapefile)
    if ~fi.exists || fi.directory then return
  endif
  
  nrs_read_shape_attributes, shapefile, att_names = att_names

  if n_elements(att_names) gt 0 then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_attribute_combobox')
    widget_control, val_fld, set_value = att_names
  endif
  
end

pro nrs_zonal_batch_use_table_toggle, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_use_table_button')
  use_table = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_outtable')
  widget_control, val_fld, sensitive = use_table
end

pro nrs_zonal_batch_handleok, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_folder')
  widget_control, val_fld, get_value = infolder
  infolder = strtrim(infolder[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_extension')
  widget_control, val_fld, get_value = img_ext
  img_ext = strtrim(img_ext[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_shapefile')
  widget_control, val_fld, get_value = shapefile
  shapefile = strtrim(shapefile[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_attribute_combobox')
  attribute = widget_info(val_fld, /combobox_gettext)
  attribute = strtrim(attribute[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_outputFolder')
  widget_control, val_fld, get_value = outfolder
  outfolder = strtrim(outfolder[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_outtable')
  widget_control, val_fld, get_value = tblfile
  tblfile = strtrim(tblfile[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_zonal_batch_use_table_button')
  use_table = widget_info(val_fld, /button_set)
  
  if strlen(infolder) eq 0 then begin
    void = error_message('Input folder not specified!', traceback = 0, /error)
    return
  endif

  if strlen(shapefile) eq 0 then begin
    void = error_message('Shapefile file not specified!', traceback = 0, /error)
    return
  endif

  if use_table then begin
    if strlen(tblfile) eq 0 then begin
      void = error_message('Missing output table name', traceback = 0, /error);
      return
    endif
  endif else void = temporary(tblfile) 
  
  ; initialise tranquilizer
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Batch calculate zonal fraction' $
    , /fast_loop $
  )
  prog_obj->Start

  nrs_zonal_fraction_batch, infolder, shapefile, attribute = attribute, out_table = tblfile $
    , out_folder = outfolder $
    , extension = img_ext $
    , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $
    prog_obj->Destroy
end

