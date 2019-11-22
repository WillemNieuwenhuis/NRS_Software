pro _build_climatology
  files = [ $
    '_build_climatology.pro', $
    'nrsclimatology__define.pro', $
    'nrs_climate_weighted_gui_eventcb.pro', $
    'nrs_climate_weighted_gui.pro', $
    'nrs_climatology_menu.pro', $
    'nrs_climatology_weighted_statistics.pro', $
    'nrs_climatology_test.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_climatology.sav',logfile = 'bin' + path_sep() + 'nrs_climatology.log'
end