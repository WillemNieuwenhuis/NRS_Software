pro _build_inform_model_tools
  ; Timeseries routines
  files = [ $
          'inform_inverse_mode.pro' $
        , 'prospect_gui_eventcb.pro' $
        , 'colorbar_n.pro' $
        , 'cw_inform_fslider.pro' $
        , 'default_soil_spectrum.pro' $
        , 'getparameterfield.pro' $
        , 'inform_aboutwindow.pro' $
        , 'inform_drawangle.pro' $
        , 'inform_prospect.pro' $
        , 'inform_prospect_single.pro' $
        , 'nrs_prospect_data.pro' $
        , 'prospect.pro' $
        , 's13aaf.pro' $
        , 'sail_background.pro' $
        , 'sail_inf.pro' $
        , 'sail_main_calc.pro' $
        , 'sail_t_so.pro' $
        , 'tav.pro' $
        , 'prospect_gui.pro' $
        , 'nrs_inform_random_permutate.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_prospect.sav', logfile = 'bin' + path_sep() + 'nrs_prospect.log'
end
