; Add to ENVI (classic) menu
pro nrs_math_tools_define_buttons, buttonInfo
  compile_opt idl2

  envi_define_menu_button, buttoninfo, value = 'Math', $
    uvalue = 'Math', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Apply scale and offset', $
    uvalue = 'Apply scale and offset', event_pro = 'nrs_scale_offset_gui', $
    ref_value = 'Math', position = 'last'

end

; Also add as ENVI 5 extensions
pro nrs_math_tools_extensions_init
  compile_opt idl2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Apply scale and offset', 'nrs_scale_offset_gui', PATH='NRS/Math tools'
end

; ENVI 5 compatibility
pro nrs_math_tools
  compile_opt idl2
end


