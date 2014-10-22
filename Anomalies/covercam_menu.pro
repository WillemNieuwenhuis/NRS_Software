; Add a menu item to the NRS menu
pro covercam_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Probability of change (CoverCam)', $
    UVALUE = 'Detect changes', EVENT_PRO = 'covercam_gui', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

; Also add as ENVI 5 extensions
pro covercam_extensions_init
  compile_opt idl2
  
  e = envi(/CURRENT)
  e.addextension, 'Probability of change (CoverCam)', 'covercam_gui', PATH='NRS'
end

; ENVI 5 compatibility
pro covercam
  compile_opt idl2
end

