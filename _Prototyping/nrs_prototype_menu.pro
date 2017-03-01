pro nrs_prototype_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Prototype', $
    uvalue = 'Prototype', /menu, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttonInfo, value = 'Batch zonal fraction', $
    uvalue = 'Batch zonal fraction', event_pro = 'nrs_zonal_batch_gui', $
    ref_value = 'Prototype', position = 'last' ;,/separator

  envi_define_menu_button, buttonInfo, value = 'RS4EBV Convert ASCII to tif', $
    uvalue = 'RS4EBV Convert ASCII to tif', event_pro = 'nrs_import_ascii_RSEBV_gui', $
    ref_value = 'Prototype', position = 'last' ;,/separator

end

pro nrs_prototype_extensions_init
  compile_opt IDL2

  e = envi(/CURRENT)
  e.addextension, 'Batch calculate Zonal fraction', 'nrs_zonal_batch_gui', PATH='NRS/Prototype'
end

; ENVI 5 compatibility
pro nrs_prototype
  compile_opt idl2
end
