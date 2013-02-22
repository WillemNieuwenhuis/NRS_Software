pro _build_topographic_tools
  ; Timeseries routines
  files = [ $
             'nrs_shortwaverad.pro' $
           , 'run_china.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_topograhic.sav', logfile = 'bin' + path_sep() + 'nrs_topograhic.log'
end
