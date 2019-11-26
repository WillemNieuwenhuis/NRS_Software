pro nrs_water_index_define_buttons, buttonInfo
  compile_opt idl2

  envi_define_menu_button, buttoninfo, value = 'Water indices', $
    uvalue = 'Water indices', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Water index', $
    uvalue = 'Water index', event_pro = 'nrs_water_index_gui', $
    ref_value = 'Water indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Batch water index', $
    uvalue = 'Batch water index', event_pro = 'nrs_water_index_batch_gui', $
    ref_value = 'Water indices', position = 'last'

end

pro nrs_water_index_extensions_init
  compile_opt IDL2

  e = ENVI(/CURRENT)
  e.AddExtension, 'Water indices', 'nrs_water_index_gui', PATH='NRS/Precipitation Tools/Water indices'
  e.AddExtension, 'Batch water indices', 'nrs_water_index_batch_gui', PATH='NRS/Precipitation Tools/Water indices'
end

pro nrs_water_index
  compile_opt idl2
end
