pro nrs_harmonic_analysis_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Timeseries', $
    uvalue = 'Timeseries', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Harmonic analysis', $
    uvalue = 'Harmonic analysis', event_pro = 'nrs_harmonic_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Harmonic composition', $
    uvalue = 'Harmonic composition', event_pro = 'nrs_harmonic_inv_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Timed aggregation', $
    uvalue = 'Timed aggregation', event_pro = 'nrs_timed_aggregation_gui', $
    ref_value = 'Timeseries', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Time interpolation', $
    uvalue = 'Time interpolation', event_pro = 'nrs_time_interpol_gui', $
    ref_value = 'Timeseries', position = 'last'

;  envi_define_menu_button, buttonInfo, value = 'Grouped statistics', $
;    uvalue = 'Grouped statistics', event_pro = 'nrs_periodstat_gui', $
;    ref_value = 'Timeseries', position = 'last'

end
