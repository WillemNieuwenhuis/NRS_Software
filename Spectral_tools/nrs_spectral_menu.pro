pro nrs_spectral_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Spectral tools', $
    uvalue = 'Spectral tools', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttonInfo, value = 'Spectrum extraction', $
    uvalue = 'Spectrum extraction', event_pro = 'nrs_aggregate_spectrum_gui', $
    ref_value = 'Spectral tools', position = 'after'

  envi_define_menu_button, buttonInfo, value = 'Spectrum extraction (polygon)', $
    uvalue = 'Spectrum extraction (polygon)', event_pro = 'nrs_aggregate_spectra_by_pol_gui', $
    ref_value = 'Spectral tools', position = 'after'

end

pro nrs_spectral_tools_extensions_init
  compile_opt idl2

  e = envi(/current)
  e.addextension, 'Spectrum extraction', 'nrs_aggregate_spectrum_gui', PATH='NRS/Spectral'
  e.AddExtension, 'Average spectrum (batch)', 'nrs_average_spectrum_gui', PATH='NRS/Spectral'
  e.addextension, 'Spectrum extraction (polygon)', 'nrs_aggregate_spectra_by_pol_gui', PATH='NRS/Spectral'
end

; ENVI 5 compatibility
pro nrs_spectral_tools
  compile_opt idl2
end

