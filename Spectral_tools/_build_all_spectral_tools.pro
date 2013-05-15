pro _build_all_spectral_tools
  files = [ $
             'nrs_average_spectrum_gui.pro' $
           , 'nrs_average_spectrum_gui_eventcb.pro' $
           , 'nrs_AverageSpectrum.pro' $
           , 'nrs_spectral_menu.pro' $
           , 'nrs_spectral_write_CSV.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_spectral_tools.sav', logfile = 'bin' + path_sep() + 'nrs_spectral_tools.log' 
end
