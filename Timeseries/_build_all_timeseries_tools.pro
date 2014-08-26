pro _build_all_timeseries_tools
  ; Timeseries routines
  files = [ $
             'nrs_correlate_profile.pro' $
           , 'nrs_correlate_profile_gui.pro' $
           , 'nrs_correlate_profile_gui_eventcb.pro' $
           , 'nrs_correlate_avg_profile.pro' $
           , 'nrs_correlate_avg_profile_gui.pro' $
           , 'nrs_correlate_avg_profile_gui_eventcb.pro' $
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
           , 'nrs_apply_season_filter_gui_event.pro' $
           , 'nrs_apply_season_filter_gui.pro' $
           , 'nrs_apply_season_filter.pro' $
           , 'nrs_interpolate_to_payment.pro' $
           , 'nrs_interpolate_to_payment_gui.pro' $
           , 'nrs_interpolate_to_payment_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_timeseries.sav', logfile = 'bin' + path_sep() + 'nrs_timeseries.log'
end
