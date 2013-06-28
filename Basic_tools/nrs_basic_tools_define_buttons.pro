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

end