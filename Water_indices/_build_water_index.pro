pro _build_water_index
  ; Timeseries routines
  files = [ $
             'nrs_water_index.pro' $
           , 'nrs_water_index_ratio.pro' $
           , 'nrs_water_index_tcwi.pro' $
           , 'nrs_water_index_gui.pro' $ 
           , 'nrs_water_index_gui_eventcb.pro' $
           , 'nrs_water_index_menu.pro' $
           
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_water_index.sav', logfile = 'bin' + path_sep() + 'nrs_water_index.log'
end
