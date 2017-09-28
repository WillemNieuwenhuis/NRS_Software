pro _build_basic_tools
  compile_opt idl2

;  _auto_build_version, name = 'basic_tools'
;  resolve_routine, 'basic_tools_version'
  
  files = [ $
             'nrs_area_numbering.pro' $
           , 'nrs_basic_tools_define_buttons.pro' $
           , 'nrs_bayesian_classify_eventCB.pro' $
           , 'nrs_bayesian_classify_gui.pro' $
           , 'nrs_bayesian_classify.pro' $
           , 'nrs_bayesian_rule.pro' $
           , 'nrs_classify_r2.pro' $
           , 'nrs_classify_r2_gui.pro' $
           , 'nrs_classify_r2_gui_eventcb.pro' $
           , 'nrs_detect_burnt.pro' $
           , 'nrs_detect_burnt_gui.pro' $
           , 'nrs_detect_burnt_gui_eventcb.pro' $
           , 'nrs_model_perf_gui.pro' $
           , 'nrs_model_perf_gui_eventcb.pro' $
           , 'nrs_model_performance.pro' $
           , 'nrs_extract_time_gui.pro' $
           , 'nrs_extract_time_gui_eventcb.pro' $
           , 'nrs_extract_by_time.pro' $
           , 'nrs_extract_by_location_shape.pro' $
           , 'nrs_import_netcdf_eventCB.pro' $
           , 'nrs_import_netcdf_gui.pro' $
           , 'nrs_netcdf.pro' $
           , 'nrs_statistics_batch.pro' $
           , 'nrs_statistics_batch_gui.pro' $
           , 'nrs_statistics_batch_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_basic_tools.sav', logfile = 'bin' + path_sep() + 'nrs_basic_tools.log'
end

