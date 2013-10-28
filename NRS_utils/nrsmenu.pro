pro _nrsmenu_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'NRS', $
    UVALUE = 'NRS', /menu, /sibling, $
    REF_VALUE = 'Basic Tools', POSITION = 'after'
    
end
