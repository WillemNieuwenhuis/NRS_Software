pro _build_all_ndvi_tools
  files = ['nrs_ndvi_indices.pro' $
           , 'nrs_drought_gui.pro' $
           , 'nrs_drought_gui_eventcb.pro' $
           , 'nrs_normalize_gui.pro' $
           , 'nrs_normalize_gui_eventcb.pro' $
           , 'nrs_find_growth_start.pro' $
           , 'nrs_growing_degree_days.pro' $
           , 'nrs_growing_degree_days_gui.pro' $
           , 'nrs_growing_degree_days_gui_eventcb.pro' $
           , 'nrs_growth_gui.pro' $
           , 'nrs_growth_gui_eventcb.pro' $
           , 'nrs_rpd.pro' $
           , 'nrs_rpd_gui.pro' $
           , 'nrs_rpd_gui_eventcb.pro' $
           , 'nrs_rpd_perc_gui.pro' $
           , 'nrs_rpd_perc_gui_eventcb.pro' $
           , 'nrs_rpd_at_location.pro' $
           , 'nrs_winter_ndvi_beck.pro' $
           , 'nrs_winter_ndvi_gui.pro' $
           , 'nrs_winter_ndvi_gui_eventcb.pro' $
           , 'nrs_ndvi_menu.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_ndvi_tools.sav', logfile = 'bin' + path_sep() + 'nrs_ndvi_tools.log'
end
