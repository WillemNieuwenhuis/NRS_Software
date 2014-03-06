pro _build_savgol_tools
  files = [ $
            'nrs_clean_savgol.pro' $
          , 'nrs_savgol_menu.pro' $
          , 'nrs_savgol_row_gui.pro' $
          , 'nrs_savgol_row_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_savgol_tools.sav',logfile = 'bin' + path_sep() + 'nrs_savgol_tools.log'
end