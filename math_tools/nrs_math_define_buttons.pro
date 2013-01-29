pro nrs_math_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Math', $
    uvalue = 'Math', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Apply scale and offset', $
    uvalue = 'Apply scale and offset', event_pro = 'nrs_scale_offset_gui', $
    ref_value = 'Math', position = 'last'

end
