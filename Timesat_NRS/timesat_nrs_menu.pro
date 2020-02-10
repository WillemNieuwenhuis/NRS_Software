; Add a menu item to the supervised classification menu
pro nrs_timesat_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Timesat (NRS)', $
    UVALUE = 'Timesat (NRS)', EVENT_PRO = 'timesat_gui', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

  envi_define_menu_button, buttonInfo, VALUE = 'Timesat batch', $
    UVALUE = 'Timesat batch', EVENT_PRO = 'nrs_timesat_batch', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

pro nrs_timesat_extensions_init
  compile_opt idl2

  e = envi(/current)
  if e eq !NULL then return

  e.AddExtension, 'Timesat (NRS)', 'timesat_gui';, PATH='Spatial'
end

; compatibility with ENVI 5.x
pro nrs_timesat
  compile_opt idl2
end

