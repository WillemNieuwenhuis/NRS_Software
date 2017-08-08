pro _build_filter_tools
  files = [ $
    'nrs_filter_cooccurence.pro' $
    , 'nrs_filter_tools_menu.pro' $
    , 'nrs_filter_cooccurence_gui.pro' $
    , 'nrs_filter_cooccurence_gui_eventcb.pro' $
    , 'nrs_clean_savgol.pro' $
    , 'nrs_savgol_row_gui.pro' $
    , 'nrs_savgol_row_gui_eventcb.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_filter_tools.sav',logfile = 'bin' + path_sep() + 'nrs_filter_tools.log'
end