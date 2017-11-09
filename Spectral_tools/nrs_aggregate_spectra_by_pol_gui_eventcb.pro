pro nrs_aggregate_spectra_by_pol_handle_input, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_input_image')
  widget_control, fld, get_value = image

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_outputFile')
  widget_control, fld, get_value = outname

  image = strtrim(image[0], 2)
  if strlen(image) eq 0 then return

  if strlen(strtrim(outname, 2)) gt 0 then return

  outname = getOutname(image, postfix = '_prof', ext = '.csv')

  widget_control, fld, set_value = outname
end

pro nrs_aggregate_spectra_by_pol_handle_shapefile, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_input')
  widget_control, fld, get_value = shapefile

  fi = file_info(shapefile)
  if (fi.exists) then begin
    nrs_openShapefile, shapefile, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info, /table_only
    nrs_close_shapes, shape = myshape
  
    combo_attr = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_attr_combo')
    sel_attr = widget_info(combo_attr, /combobox_gettext)

    selectable = attr_info.(0)
    widget_control, combo_attr, set_value = selectable
    
    ix = where(selectable eq sel_attr, cnt)
    if cnt eq 1 then $
      widget_control, combo_attr, set_combobox_select = ix[0]
 endif
end


pro nrs_aggregate_spectra_by_pol_handleOK, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_input_image')
  widget_control, fld, get_value = image

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_input')
  widget_control, fld, get_value = shapefile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_attr_combo')
  attribute = widget_info(fld, /combobox_gettext)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_table_type')
  widget_control, val_fld, get_value = table_type

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_outputFile')
  widget_control, fld, get_value = outname

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectra_by_pol_aggr_func')
  aggr_func = widget_info(fld, /combobox_gettext)

  image = strtrim(image[0], 2)
  if strlen(image) eq 0 then begin
    void = error_message('No spectral image specified', traceback = 0, /error)
    return
  endif

  shapefile = strtrim(shapefile[0], 2)
  if strlen(shapefile) eq 0 then begin
    void = error_message('No polygon shapefile specified', traceback = 0, /error)
    return
  endif

  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Calculate stack statistics" $
    , /fast_loop $
    )

  ; calculate the spectrum
  nrs_aggregate_spectra_by_pol, shapefile, image $
    , aggr_func = aggr_func $
    , xytable = table_type $
    , attribute = attribute $
    , verbose = verbose $
    , outname = outname $
    , prog_obj = prog_obj, cancelled = cancelled
  
  if obj_valid(prog_obj) then $
    prog_obj->destroy

  if ~cancelled then $
    void = dialog_message('Finished', title='Spectrum extraction', /information)

end