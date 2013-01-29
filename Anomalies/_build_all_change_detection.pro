pro _build_all
  files = ['anomaly_gui.pro' $
           , 'anomaly_gui_eventcb.pro' $
           , 'nrs_detect_anomalies.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin\anomaly_gui.sav'

  files = ['change_detection_gui.pro' $
           , 'change_detection_gui_eventcb.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin\change_detection_gui.sav'

  ; needs nrs_utils.sav, and areanumbering (in tools)
  files = ['change_detection_v2_gui.pro' $
           , 'change_detection_v2_gui_eventcb.pro' $
           , 'nrs_ndvi_stddev.pro' $
           , 'nrs_ndvi_change_detection.pro' $
          ]
  _auto_build, files, 'bin\change_detection_v2_gui.sav'
  
end
