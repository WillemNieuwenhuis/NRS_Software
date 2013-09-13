pro _build_all_precipitation_tools
  files = ['nrs_precipitation_index.pro' $
           , 'nrs_spi_gui.pro' $
           , 'nrs_spi_gui_eventcb.pro' $
           , 'nrs_classify_spi.pro' $
           , 'nrs_classify_spi_gui.pro' $
           , 'nrs_classify_spi_gui_eventcb.pro' $
           , 'nrs_climind_prcptot.pro' $
           , 'nrs_climind_prcptot_gui.pro' $
           , 'nrs_climind_prcptot_gui_eventcb.pro' $
           , 'nrs_climind_rnn.pro' $
           , 'nrs_climind_rnn_gui.pro' $
           , 'nrs_climind_rnn_gui_eventcb.pro' $
           , 'nrs_climind_rx5day.pro' $
           , 'nrs_climind_rx5day_gui.pro' $
           , 'nrs_climind_rx5day_gui_eventcb.pro' $
           , 'nrs_precipitation_menu.pro' $
           , 'nrs_rainfall_consecutive.pro' $
           , 'nrs_rainfall_consecutive_gui.pro' $
           , 'nrs_rainfall_consecutive_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_precipitation_tools.sav', logfile = 'bin' + path_sep() + 'nrs_precipitation_tools.log' 
end
