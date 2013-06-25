pro nrs_water_index_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Water indices', $
    uvalue = 'Water indices', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Water index', $
    uvalue = 'Water index', event_pro = 'nrs_water_index_gui', $
    ref_value = 'Water indices', position = 'last'

end
