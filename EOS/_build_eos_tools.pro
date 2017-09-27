pro _build_eos_tools
  files = [ $
    'nrs_modis_conversion_tools.pro', $
    'nrs_eos_tools_menu.pro' $
    ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_eos_tools.sav',logfile = 'bin' + path_sep() + 'nrs_eos_tools.log'
end