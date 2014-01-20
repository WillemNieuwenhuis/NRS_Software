pro nrs_spectral_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Spectral tools', $
    uvalue = 'Spectral tools', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttonInfo, value = 'Spectrum Extraction', $
    uvalue = 'Spectrum Extraction', event_pro = 'nrs_aggregate_spectrum_gui', $
    ref_value = 'Spectral tools', position = 'after'

end
