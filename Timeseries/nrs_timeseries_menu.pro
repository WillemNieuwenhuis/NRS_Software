pro nrs_timeseries_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Timeseries', $
    uvalue = 'Timeseries', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Harmonic analysis', $
    uvalue = 'Harmonic analysis', event_pro = 'nrs_harmonic_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Harmonic composition', $
    uvalue = 'Harmonic composition', event_pro = 'nrs_harmonic_inv_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Grouping by time', $
    uvalue = 'Timed grouping', event_pro = 'nrs_timed_grouping_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Time interpolation', $
    uvalue = 'Time interpolation', event_pro = 'nrs_time_interpol_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Correlate with profile', $
    uvalue = 'Correlate with profile', event_pro = 'nrs_correlate_profile_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Sensitivity of correlate profile', $
    uvalue = 'Sensitivity of correlate profile', event_pro = 'nrs_correlate_avg_profile_gui', $
    ref_value = 'Timeseries', position = 'last'
    
  envi_define_menu_button, buttonInfo, value = 'Apply season filter', $
    uvalue = 'Apply season filter', event_pro = 'nrs_apply_season_filter_gui', $
    ref_value = 'Timeseries', position = 'last', /separator

  envi_define_menu_button, buttonInfo, value = 'Determine pay percentage', $
    uvalue = 'Determine pay percentage', event_pro = 'nrs_interpolate_to_payment_gui', $
    ref_value = 'Timeseries', position = 'last'
    
  envi_define_menu_button, buttonInfo, value = 'Calculate payment average', $
    uvalue = 'Calculate payment average', event_pro = 'nrs_payment_averages_gui', $
    ref_value = 'Timeseries', position = 'last'

end
    
; Also add as ENVI 5 extensions
pro nrs_timeseries_extensions_init
  compile_opt idl2
  
  e = envi(/CURRENT)
  e.addextension, 'Harmonic analysis', 'nrs_harmonic_gui', PATH='NRS/Timeseries'
  e.addextension, 'Harmonic composition', 'nrs_harmonic_inv_gui', PATH='NRS/Timeseries'
  e.addextension, 'Aggregation by time', 'nrs_timed_aggregation_gui', PATH='NRS/Timeseries'
  e.addextension, 'Time interpolation', 'nrs_time_interpol_gui', PATH='NRS/Timeseries'
  e.addextension, 'Correlate with profile', 'nrs_correlate_profile_gui', PATH='NRS/Timeseries'
  e.addextension, 'Sensitivity of correlate profile', 'nrs_correlate_avg_profile_gui', PATH='NRS/Timeseries'
  e.addextension, 'Apply season filter', 'nrs_apply_season_filter_gui', PATH='NRS/Timeseries'
  e.addextension, 'Determine pay percentage', 'nrs_interpolate_to_payment_gui', PATH='NRS/Timeseries'
end

; ENVI 5 compatibility
pro nrs_timeseries
  compile_opt idl2
end
