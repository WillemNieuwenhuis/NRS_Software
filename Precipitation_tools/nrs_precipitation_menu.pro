pro nrs_precipitation_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Precipitation', $
    uvalue = 'Precipitation', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Calculate SPI', $
    uvalue = 'Calculate SPI', event_pro = 'nrs_spi_gui', $
    ref_value = 'Precipitation', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Classify SPI', $
    uvalue = 'Classify SPI', event_pro = 'nrs_classify_spi_gui', $
    ref_value = 'Precipitation', position = 'last'

end
