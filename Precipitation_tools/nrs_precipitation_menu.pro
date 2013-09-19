pro nrs_precipitation_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Precipitation', $
    uvalue = 'Precipitation', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Climate indices', $
    uvalue = 'Climate indices', /menu, $
    ref_value = 'Precipitation', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Determine historical 95th percentile', $
    uvalue = 'Determine historical 95th percentile', event_pro = 'nrs_climind_perc_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Annual total wet day precipitation', $
    uvalue = 'Annual total wet day precipitation', event_pro = 'nrs_climind_r95p_gui', $
    ref_value = 'Climate indices', position = 'last', /sep

  envi_define_menu_button, buttoninfo, value = 'Maximum 5-day precipitation', $
    uvalue = 'Maximum 5-day precipitation', event_pro = 'nrs_climind_rx5day_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Maximum consecutive wet/dry', $
    uvalue = 'Maximum consecutive wet/dry', event_pro = 'nrs_rainfall_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Count wet days', $
    uvalue = 'Count wet days', event_pro = 'nrs_climind_rnn_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Annual total wet day precipitation', $
    uvalue = 'Annual total wet day precipitation', event_pro = 'nrs_climind_rnn_gui', $
    ref_value = 'Climate indices', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Calculate SPI', $
    uvalue = 'Calculate SPI', event_pro = 'nrs_spi_gui', $
    ref_value = 'Precipitation', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Classify SPI', $
    uvalue = 'Classify SPI', event_pro = 'nrs_classify_spi_gui', $
    ref_value = 'Precipitation', position = 'last'

end
