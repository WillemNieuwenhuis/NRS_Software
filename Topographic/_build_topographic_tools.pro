pro _build_topographic_tools
  ; Timeseries routines
  files = [ $
             'nrs_shortwaverad.pro' $
           , 'run_china.pro' $
           , 'nrs_terrain_pos_deepestdescent.pro' $
           , 'nrs_terrain_pos_gui_eventcb.pro' $
           , 'nrs_terrain_pos_gui.pro' $
           , 'nrs_terrain_pos_thinmat_kak.pro' $
           , 'nrs_terrain_pos_thinmat.pro' $
           , 'nrs_terrain_pos.pro' $
           , 'nrs_topograpic_menu.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_topographic.sav', logfile = 'bin' + path_sep() + 'nrs_topographic.log'
end
