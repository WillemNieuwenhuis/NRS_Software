pro nrs_class_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Classification', $
    uvalue = 'Classification', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Separability analysis', $
    uvalue = 'Separability analysis', event_pro = 'nrs_separability_gui', $
    ref_value = 'Classification', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Separability analysis (multi)', $
    uvalue = 'Separability analysis (multi)', event_pro = 'nrs_separmulti_gui', $
    ref_value = 'Classification', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Automate ISOdata cluster', $
    uvalue = 'Automate ISOdata cluster', event_pro = 'nrs_autoclus_gui', $
    ref_value = 'Classification', position = 'last'

end
