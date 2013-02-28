pro _build_change_detection
  files = ['anomaly_gui.pro' $
           , 'anomaly_gui_eventcb.pro' $
           , 'nrs_detect_anomalies.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'anomaly_gui.sav', logfile = 'bin' + path_sep() + 'anomaly_gui.log'

  files = ['change_detection_gui.pro' $
           , 'change_detection_gui_eventcb.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'change_detection_gui.sav', logfile = 'bin' + path_sep() + 'change_detection_gui.log'

  ; needs nrs_utils.sav, and nrs_basic_tools.sav
  files = ['change_detection_v2_gui.pro' $
           , 'change_detection_v2_gui_eventcb.pro' $
           , 'nrs_ndvi_stddev.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'change_detection_v2_gui.sav', logfile = 'bin' + path_sep() + 'change_detection_v2_gui.log'
  
end
