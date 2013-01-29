pro nrs_csy_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Coordinate system', $
    uvalue = 'Coordinate system', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Generate coordinate images', $
    uvalue = 'Generate coordinate images', event_pro = 'nrs_ll_grid_gui', $
    ref_value = 'Coordinate system', position = 'last'

end
