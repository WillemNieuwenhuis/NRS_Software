pro _nrsmenu_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'NRS', $
    UVALUE = 'NRS', /menu, /sibling, $
    REF_VALUE = 'Basic Tools', POSITION = 'after'
    
end

; For compatibility with ENVI 5, to hide the nrs_utils as extension
pro _nrsmenu
  compile_opt idl2
end

; For compatibility with ENVI 5, to hide the nrs_utils as extension
; For this library, it is empty (no exposed apps)
pro nrs_utils_extensions_init
  compile_opt idl2
end

; For compatibility with ENVI 5, a procedure with the name of the save file must exist
; For this library, it is empty (no exposed apps)
pro nrs_utils
  compile_opt idl2
end