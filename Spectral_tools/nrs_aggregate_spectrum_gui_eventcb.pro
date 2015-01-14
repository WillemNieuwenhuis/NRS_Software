pro nrs_aggregate_spectrum_handle_input, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_input_image')
  widget_control, fld, get_value = image
  
  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_outputFile')
  widget_control, fld, get_value = outname
  
  image = strtrim(image[0], 2)
  if strlen(image) eq 0 then return
  
  if strlen(strtrim(outname, 2)) gt 0 then return
  
  outname = getOutname(image, postfix = '_prof', ext = '.csv')
  
  widget_control, fld, set_value = outname
end

pro nrs_aggregate_spectrum_handleOK, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_input_image')
  widget_control, fld, get_value = image
  
  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_input_table')
  widget_control, fld, get_value = pnt_tbl
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_table_type')
  widget_control, val_fld, get_value = table_type

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_outputFile')
  widget_control, fld, get_value = outname
  
  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_kernel_combo')
  kern = widget_info(fld, /combobox_gettext)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_spectrum_aggr_func')
  aggr_func = widget_info(fld, /combobox_gettext)

  image = strtrim(image[0], 2)
  if strlen(image) eq 0 then begin
    void = error_message('No spectral image specified', traceback = 0, /error)
    return
  endif
  
  pnt_tbl = strtrim(pnt_tbl[0], 2)
  if strlen(pnt_tbl) eq 0 then begin
    void = error_message('No location table specified', traceback = 0, /error)
    return
  endif
  
      ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate stack statistics" $
                        , /fast_loop $
                        )
  
  ; calculate the spectrum
  nrs_aggregate_spectra, pnt_tbl, image $
                       , outname = outname $
                       , kernel = kern, aggr_func = aggr_func $
                       , xytable = table_type $
                       , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $
    prog_obj->destroy
  
  if ~cancelled then $
    void = dialog_message('Finished', title='Spectrum extraction', /information)

end