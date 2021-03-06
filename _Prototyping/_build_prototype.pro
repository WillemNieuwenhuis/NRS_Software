pro _build_prototype
  compile_opt idl2, logical_predicate

  files = [ $
    'nrs_prototype_menu.pro' $
    , 'nrs_zonal_fraction.pro' $
    , 'nrs_zonal_batch_gui.pro' $
    , 'nrs_zonal_batch_gui_eventcb.pro' $
    , 'nrs_import_ascii_rsebv_gui.pro' $
    , 'nrs_import_ascii_rsebv.pro' $
    , 'nrs_gwp_convert.pro' $
    , 'nrs_gwp_tiff_gui.pro' $
    , 'nrs_gwp_tiff_gui_eventcb.pro' $
    , 'nrs_class_membership.pro' $
    , 'nrs_class_membership_gui.pro' $
    , 'nrs_class_membership_gui_eventcb.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_prototype.sav', logfile = 'bin' + path_sep() + 'nrs_prototype.log'

end
