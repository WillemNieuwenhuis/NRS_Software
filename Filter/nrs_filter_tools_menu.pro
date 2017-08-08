; Add to ENVI (classic) menu
pro nrs_filter_tools_define_buttons, buttonInfo
  compile_opt idl2

  envi_define_menu_button, buttoninfo, value = 'Filter', $
    uvalue = 'Filter', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Texture co-occurrence', $
    uvalue = 'Texture co-occurrence', event_pro = 'nrs_filter_cooccurence_gui', $
    ref_value = 'Filter', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Apply Savitzky-Golay (by row)', $
    uvalue = 'Apply Savitzky-Golay (by row)', event_pro = 'nrs_savgol_row_gui', $
    ref_value = 'Filter', position = 'last'

end

; Also add as ENVI 5 extensions
pro nrs_filter_tools_extensions_init
  compile_opt idl2

  e = ENVI(/CURRENT)
  e.AddExtension, 'Texture co-occurrence', 'nrs_filter_cooccurence_gui', PATH='NRS/Filter'
  e.AddExtension, 'Apply Savitzky-Golay (by row)', 'nrs_savgol_row_gui', PATH='NRS/Filter'
end

; ENVI 5 compatibility
pro nrs_filter_tools
  compile_opt idl2
end


