pro _build_all_precipitation_tools
  files = ['nrs_precipitation_index.pro' $
           , 'nrs_spi_gui.pro' $
           , 'nrs_spi_gui_eventcb.pro' $
           , 'nrs_classify_spi.pro' $
           , 'nrs_classify_spi_gui.pro' $
           , 'nrs_classify_spi_gui_eventcb.pro' $
           , 'nrs_precipitation_menu.pro' $
          ]
  _auto_build, files, 'bin\nrs_precipitation_tools.sav', logfile = 'bin\nrs_precipitation_tools.log' 
end
