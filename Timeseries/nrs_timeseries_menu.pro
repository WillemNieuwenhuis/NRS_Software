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

  envi_define_menu_button, buttonInfo, value = 'Aggregation by time', $
    uvalue = 'Timed aggregation', event_pro = 'nrs_timed_aggregation_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Time interpolation', $
    uvalue = 'Time interpolation', event_pro = 'nrs_time_interpol_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Correlate with profile', $
    uvalue = 'Correlate with profile', event_pro = 'nrs_correlate_profile_gui', $
    ref_value = 'Timeseries', position = 'last'

end
