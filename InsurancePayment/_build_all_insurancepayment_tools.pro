pro _build_all_insurancepayment_tools
  ; Timeseries routines
  files = [ $
             'nrs_apply_payment_bias.pro' $
           , 'nrs_apply_season_filter_gui_eventcb.pro' $
           , 'nrs_apply_season_filter_gui.pro' $
           , 'nrs_apply_season_filter.pro' $
           , 'nrs_average_payment.pro' $
           , 'nrs_interpolate_to_payment_gui_eventcb.pro' $
           , 'nrs_interpolate_to_payment_gui.pro' $
           , 'nrs_interpolate_to_payment.pro' $
           , 'nrs_zonal_percentiles_gui_eventcb.pro' $
           , 'nrs_zonal_percentiles_gui.pro' $
           , 'nrs_zonal_percentiles.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'insurancepayment.sav', logfile = 'bin' + path_sep() + 'insurancepayment.log'
end
