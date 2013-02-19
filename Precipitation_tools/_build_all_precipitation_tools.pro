pro _build_all_precipitation_tools
  files = ['nrs_precipitation_index.pro' $
           , 'nrs_spi_gui.pro' $
           , 'nrs_spi_gui_eventcb.pro' $
           , 'nrs_classify_spi.pro' $
           , 'nrs_classify_spi_gui.pro' $
           , 'nrs_classify_spi_gui_eventcb.pro' $
           , 'nrs_precipitation_menu.pro' $
           , 'nrs_rainfall_consecutive.pro' $
           , 'nrs_rainfall_consecutive_gui.pro' $
           , 'nrs_rainfall_consecutive_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_precipitation_tools.sav', logfile = 'bin' + path_sep() + 'nrs_precipitation_tools.log' 
end
