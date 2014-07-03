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

; Also add as ENVI 5 extensions
pro nrs_class_tools_extensions_init
  compile_opt idl2
  
  e = envi(/CURRENT)
  e.addextension, 'Automate ISOdata cluster', 'nrs_autoclus_gui', PATH='NRS/Classify tools'
  e.addextension, 'Separability analysis', 'nrs_separability_gui', PATH='NRS/Classify tools'
  e.addextension, 'Separability analysis (multi)', 'nrs_separmulti_gui', PATH='NRS/Classify tools'
end

; ENVI 5 compatibility
pro nrs_class_tools
  compile_opt idl2
end

