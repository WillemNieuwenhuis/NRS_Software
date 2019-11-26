; Add to ENVI (classic) menu
pro nrs_climatology_define_buttons, buttonInfo
  compile_opt idl2

  envi_define_menu_button, buttoninfo, value = 'Climatology', $
    uvalue = 'Climatology', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Statistics', $
    uvalue = 'Statistics', event_pro = 'nrs_climate_weighted_gui', $
    ref_value = 'Climatology', position = 'last'


end

; Also add as ENVI 5 extensions
pro nrs_climatology_extensions_init
  compile_opt idl2

  e = ENVI(/CURRENT)
  e.AddExtension, 'Statistics', 'nrs_climate_weighted_gui', PATH='NRS/Climatology'
end

; ENVI 5 compatibility
pro nrs_climatology
  compile_opt idl2
end


