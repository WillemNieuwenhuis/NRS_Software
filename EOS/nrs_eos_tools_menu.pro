; Add to ENVI (classic) menu
pro nrs_eos_tools_define_buttons, buttonInfo
  compile_opt idl2

  envi_define_menu_button, buttoninfo, value = 'EOS Convert', $
    uvalue = 'Filter', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Convert MODIS Grid', $
    uvalue = 'Texture co-occurrence', event_pro = 'nrs_modis_convert_grid_gui', $
    ref_value = 'EOS Convert', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Batch convert MODIS Grid', $
    uvalue = 'Texture co-occurrence', event_pro = 'nrs_modis_batch_convert_grid_gui', $
    ref_value = 'EOS Convert', position = 'last'


end

; Also add as ENVI 5 extensions
pro nrs_eos_tools_extensions_init
  compile_opt idl2

  e = envi(/current)
  e.AddExtension, 'Convert MODIS Grid', 'nrs_modis_convert_grid_gui', PATH='NRS/EOS'
  e.AddExtension, 'Batch convert MODIS Grid', 'nrs_modis_batch_convert_grid_gui', PATH='NRS/EOS'
end

; ENVI 5 compatibility
pro nrs_eos_tools
  compile_opt idl2
end


