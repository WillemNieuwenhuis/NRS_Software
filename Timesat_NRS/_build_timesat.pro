pro _build_timesat
  files = [ 'savgol.pro' $
           , 'spike.pro' $
           , 'timesat_nrs_menu.pro' $
           , 'timesat_gui.pro' $
           , 'timesat_gui_eventcb.pro' $
           , 'timesat_v11.pro' $
           , 'nrs_timesat_batch.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_timesat.sav', logfile = 'bin' + path_sep() + 'nrs_timesat.log'

end
