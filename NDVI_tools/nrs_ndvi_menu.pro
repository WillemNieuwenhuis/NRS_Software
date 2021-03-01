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

  envi_define_menu_button, buttonInfo, value = 'Growing degree days (GDD)', $
    uvalue = 'Growing degree days (GDD)', event_pro = 'nrs_growing_degree_days_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Find start of growing season', $
    uvalue = 'Find start of growing season', event_pro = 'nrs_growth_gui', $
    ref_value = 'NDVI', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Extract BRDF flags', $
    uvalue = 'Extract BRDF flags', event_pro = 'nrs_extract_brdf_flags_gui', $
    ref_value = 'NDVI', position = 'last'

end

; Also add as ENVI 5 extensions
pro nrs_ndvi_tools_extensions_init
  compile_opt idl2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Normalize indices', 'nrs_normalize_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'Drought index', 'nrs_normalize_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'RPD (GWI)', 'nrs_rpd_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'GWI Threshold', 'nrs_rpd_perc_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'Winter NDVI', 'nrs_winter_ndvi_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'Find start of growing season', 'nrs_growth_gui', PATH='NRS/NDVI tools'
  e.AddExtension, 'Extract BRDF flags', 'nrs_extract_brdf_flags_gui', PATH='NRS/NDVI tools'
end

; ENVI 5 compatibility
pro nrs_ndvi_tools
  compile_opt idl2
end


