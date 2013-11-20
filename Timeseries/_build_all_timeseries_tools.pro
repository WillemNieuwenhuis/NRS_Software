pro _build_all_timeseries_tools
  ; Timeseries routines
  files = [  'nrs_correlate_profile.pro' $
           , 'nrs_correlate_profile_gui.pro' $
           , 'nrs_correlate_profile_gui_eventcb.pro' $
           , 'nrs_harmonic_analysis.pro' $
           , 'nrs_harmonic_gui.pro' $
           , 'nrs_harmonic_gui_eventcb.pro' $
           , 'nrs_harmonic_inv_gui.pro' $
           , 'nrs_harmonic_inv_gui_eventcb.pro' $
           , 'nrs_timeseries_menu.pro' $
           , 'nrs_timed_aggregation.pro' $
           , 'nrs_timed_aggregation_gui.pro' $
           , 'nrs_timed_aggregation_gui_eventcb.pro' $
           , 'nrs_time_interpol_gui_eventcb.pro' $
           , 'nrs_time_interpol_gui.pro' $
           , 'nrs_time_interpolate.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_timeseries.sav', logfile = 'bin' + path_sep() + 'nrs_timeseries.log'
end
