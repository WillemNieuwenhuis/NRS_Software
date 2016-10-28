pro _build_all_amapp
  files = ['nrs_stratify_gui.pro' $
           , 'nrs_stratify_gui_eventcb.pro' $
           , 'strat_stratify.pro' $
;           , 'nrs_amapp_menu.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_stratify.sav', logfile = 'bin' + path_sep() + 'nrs_stratify.log'
end