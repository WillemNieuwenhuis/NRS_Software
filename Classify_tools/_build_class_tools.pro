pro _build_class_tools
  files = [ $
            'nrs_class_tools_menu.pro' $
          , 'nrs_auto_cluster.pro' $
          , 'nrs_autoclus_gui.pro' $
          , 'nrs_autoclus_gui_eventcb.pro' $
          , 'nrs_class_separability.pro' $
          , 'nrs_separability_gui.pro' $
          , 'nrs_separability_gui_eventcb.pro' $
          , 'nrs_separmulti_gui.pro' $
          , 'nrs_separmulti_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_class_tools.sav',logfile = 'bin' + path_sep() + 'nrs_class_tools.log'
end