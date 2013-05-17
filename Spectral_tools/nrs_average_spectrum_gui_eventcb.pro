pro nrs_handle_average_spectrum_toggle, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_toggle')
  isOn =  widget_info(fld, /button_set)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_threshold_panel')
  widget_control, fld, sensitiv = isOn
end

pro nrs_average_spectrum_handleOK, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_image_folder')
  widget_control, fld, get_value = folder_img
  
  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_shape_folder')
  widget_control, fld, get_value = folder_shp
  
  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_kernel_combo')
  kern = widget_info(fld, /combobox_gettext)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_toggle')
  isOn =  widget_info(fld, /button_set)

  fld = widget_info(event.top, find_by_uname = 'nrs_average_spectrum_threshold')
  widget_control, fld, get_value = thresh

	threshold = isOn ? float(thresh[0]) / 100 : 0
	folder_img = strtrim(folder_img[0], 2)
  folder_shp = strtrim(folder_shp[0], 2)
  
  hymaps = nrs_find_images(folder_img, '.*', ext = '*', /exclude_hdr)
  if n_elements(hymaps) eq 0 then begin
    void = error_message('No images found, select another folder', traceback = 0, /error)
    return
  endif
  
  shapes = nrs_find_images(folder_shp, '.*', ext = 'shp')
  if n_elements(shapes) eq 0 then begin
    void = error_message('No shapefiles found, select another folder', traceback = 0, /error)
    return
  endif

	; calculate the spectrum
	nrs_average_spectrum, shapes, hymaps, output, threshold

	ans = Dialog_Message('Finished', title='Spectrum Extraction', /information)

end