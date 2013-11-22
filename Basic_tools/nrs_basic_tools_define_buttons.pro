pro nrs_basic_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Classify R2', $
    uvalue = 'Classify R2', event_pro = 'nrs_classify_r2_gui', $
    ref_value = 'NRS', position = 'first'

  envi_define_menu_button, buttoninfo, value = 'Model performance', $
    uvalue = 'Model performance', event_pro = 'nrs_model_perf_gui', $
    ref_value = 'NRS', position = 'first'

  envi_define_menu_button, buttoninfo, value = 'Extract by time', $
    uvalue = 'Extract by time', event_pro = 'nrs_extract_time_gui', $
    ref_value = 'NRS', position = 'first'

  envi_define_menu_button, buttoninfo, value = 'Import netCDF', $
    uvalue = 'Import netCDF', event_pro = 'nrs_import_netcdf_gui', $
    ref_value = 'NRS', position = 'first'

  envi_define_menu_button, buttoninfo, value = 'Spatial statistics (batch)', $
    uvalue = 'Spatial statistics (batch)', event_pro = 'nrs_statistics_batch_gui', $
    ref_value = 'NRS', position = 'first'

  envi_define_menu_button, buttoninfo, value = 'Detect burnt periods', $
    uvalue = 'Detect burnt periods', event_pro = 'nrs_detect_burnt_gui', $
    ref_value = 'NRS', position = 'last'

end

pro nrs_basic_tools_extensions_init
  compile_opt IDL2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Batch statistics', 'nrs_statistics_batch_gui', PATH='NRS/Basic tools'
  e.AddExtension, 'Determine model performance', 'nrs_model_perf_gui', PATH='NRS/Basic tools'
  e.AddExtension, 'Import netCDF files', 'nrs_import_netcdf_gui', PATH='NRS/Basic tools'
  e.AddExtension, 'Extract by time / location', 'nrs_extract_time_gui', PATH='NRS/Basic tools'
  e.AddExtension, 'Classify R2', 'nrs_classify_r2_gui', PATH='NRS/Basic tools'
  e.AddExtension, 'Detect burnt periods', 'nrs_detect_burnt_gui', PATH='NRS/Basic tools'
end

; For ENVI 5 compatibility
pro nrs_basic_tools
end