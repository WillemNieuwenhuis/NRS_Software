pro _build_prototype
  compile_opt idl2, logical_predicate

  files = [ $
    'nrs_prototype_menu.pro' $
    , 'nrs_zonal_fraction.pro' $
    , 'nrs_zonal_batch_gui.pro' $
    , 'nrs_zonal_batch_gui_eventcb.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_prototype.sav', logfile = 'bin' + path_sep() + 'nrs_prototype.log'

end
