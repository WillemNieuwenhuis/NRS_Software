pro _build_stack_tools
  files = [ $
             'nrs_stack_menu.pro' $
           , 'nrs_unstack_image.pro' $
           , 'nrs_unstack_gui.pro' $
           , 'nrs_unstack_gui_eventCB.pro' $
           , 'nrs_stack_gui.pro' $
           , 'nrs_stack_gui_eventCB.pro' $
           , 'nrs_stack_aggregation_gui.pro' $
           , 'nrs_stack_aggregation_gui_eventcb.pro' $
           , 'nrs_stack_quartiles.pro' $
           , 'nrs_stack_quartiles_gui.pro' $
           , 'nrs_stack_quartiles_gui_eventcb.pro' $
           , 'nrs_bias_removal.pro' $
           , 'nrs_bias_removal_gui.pro' $
           , 'nrs_bias_removal_gui_eventCB.pro' $
           , 'nrs_stack_statistics.pro' $
           , 'nrs_stack_statistics_gui.pro' $
           , 'nrs_stack_statistics_gui_eventCB.pro' $
           , 'nrs_compare_stacks.pro' $
           , 'nrs_stack_compare_gui.pro' $
           , 'nrs_stack_compare_gui_eventcb.pro' $
;           , 'nrs_aggregate_gui_eventCB.pro' $
;           , 'nrs_aggregate_gui.pro' $
;           , 'nrs_grouped_aggregation.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_stack_tools.sav', logfile = 'bin' + path_sep() + 'nrs_stack_tools.log'
end
