pro _build_all_spectral_tools
  compile_opt idl2
  
  resolve_routine, '_auto_build'
  _auto_build_version, name = 'spectral_tools'
  resolve_routine, 'spectral_tools_version'

  files = [ $
             'nrs_aggregate_spectrum_gui.pro.pro' $
           , 'nrs_aggregate_spectrum_gui_eventcb.pro' $
           , 'nrs_aggregate_spectra.pro' $
           , 'nrs_spectral_menu.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_spectral_tools.sav', logfile = 'bin' + path_sep() + 'nrs_spectral_tools.log' 
end
