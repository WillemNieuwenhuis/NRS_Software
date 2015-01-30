;+
; Add menu and extension when running in ENVI
; :author: nieuwenhuis
;-

pro nrs_giacis_gui_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Timesat (GiaCIS)', $
    UVALUE = 'Timesat (GiaCIS)', EVENT_PRO = 'nrs_giacis_gui', $
    REF_VALUE = 'NRS', POSITION = 'last', /SEPARATOR

end

pro nrs_giacis_gui_extensions_init
  compile_opt idl2

  e = envi(/current)
  if e eq !NULL then return

  e.addextension, 'Timesat (GiaCIS)', 'nrs_giacis_gui';, PATH='Spatial'
end

