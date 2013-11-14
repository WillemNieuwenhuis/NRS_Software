pro nrs_precipitation_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Precipitation', $
    uvalue = 'Precipitation', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Climate indices', $
    uvalue = 'Climate indices', /menu, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Determine historical 95th percentile', $
    uvalue = 'Determine historical 95th percentile', event_pro = 'nrs_climind_perc_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = '17,18 - Rx1day,Rx5day: Maximum 1-day/5-day precipitation', $
    uvalue = 'Maximum 5-day precipitation', event_pro = 'nrs_climind_rx5day_gui', $
    ref_value = 'Climate indices', position = 'last', /sep

  envi_define_menu_button, buttoninfo, value = '22 - Rnn: Count wet days', $
    uvalue = 'Count wet days', event_pro = 'nrs_climind_rnn_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = '23,24 - CDD,CWD: Maximum consecutive dry/wet', $
    uvalue = 'Maximum consecutive wet/dry', event_pro = 'nrs_climind_cdd_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = '25 - r95p: Annual total very wet day precipitation', $
    uvalue = 'Annual total very wet day precipitation', event_pro = 'nrs_climind_r95p_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = '27 - PRCPTOT: Annual total wet day precipitation', $
    uvalue = 'Annual total wet day precipitation', event_pro = 'nrs_climind_ptot_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Calculate SPI', $
    uvalue = 'Calculate SPI', event_pro = 'nrs_spi_gui', $
    ref_value = 'Precipitation', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Classify SPI', $
    uvalue = 'Classify SPI', event_pro = 'nrs_classify_spi_gui', $
    ref_value = 'Precipitation', position = 'last'
end

; Also add as ENVI 5 extensions
pro nrs_precipitation_tools_extensions_init
  compile_opt idl2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Determine historical 95th percentile', 'nrs_climind_perc_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, '17,18 - Rx1day,Rx5day: Maximum 5-day precipitation', 'nrs_climind_rx5day_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, '22 - Rnn: Count wet days', 'nrs_climind_rnn_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, '23,24 - CDD,CWD: Maximum consecutive wet/dry', 'nrs_climind_cdd_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, '25 - r95p: Annual total very wet day precipitation', 'nrs_climind_r95p_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, '27 - PRCPTOT: Annual total wet day precipitation', 'nrs_climind_ptot_gui', PATH='NRS/Precipitation tools/Climate indices'
  e.AddExtension, 'Calculate SPI', 'nrs_spi_gui', PATH='NRS/Precipitation tools'
  e.AddExtension, 'Classify SPI', 'nrs_classify_spi_gui', PATH='NRS/Precipitation tools'
end

; ENVI 5 compatibility
pro nrs_precipitation_tools
  compile_opt idl2
end

