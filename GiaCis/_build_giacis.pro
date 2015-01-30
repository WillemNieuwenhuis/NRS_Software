pro _build_giacis
  files = [ 'nrs_savgol.pro' $
           , 'nrs_spike.pro' $
           , 'nrs_giacis_gui.pro' $
           , 'nrs_giacis_gui_eventcb.pro' $
           , 'nrs_giacis_menu.pro' $
           , 'nrs_timesat_idl.pro' $
           , 'nrs_timesat_utils.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'giacis_gui.sav', logfile = 'bin' + path_sep() + 'giacis_gui.log'

end
