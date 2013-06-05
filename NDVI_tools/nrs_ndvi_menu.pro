pro nrs_ndvi_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'NDVI', $
    uvalue = 'NDVI', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Normalize indices', $
    uvalue = 'Normalize indices', event_pro = 'nrs_normalize_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Drought index', $
    uvalue = 'Drought index', event_pro = 'nrs_drought_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'RPD (GWI)', $
    uvalue = 'RPD', event_pro = 'nrs_rpd_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'GWI Threshold', $
    uvalue = 'GWI Threshold', event_pro = 'nrs_rpd_perc_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Winter NDVI', $
    uvalue = 'Winter NDVI', event_pro = 'nrs_winter_ndvi_gui', $
    ref_value = 'NDVI', position = 'last'

end
