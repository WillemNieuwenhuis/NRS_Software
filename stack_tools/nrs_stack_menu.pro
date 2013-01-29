pro nrs_stack_tools_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Stack', $
    uvalue = 'Stack', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'after'

  envi_define_menu_button, buttoninfo, value = 'Compare stacks', $
    uvalue = 'Compare stacks', event_pro = 'nrs_stack_compare_gui', $
    ref_value = 'Stack', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Remove bias', $
    uvalue = 'Remove Bias', event_pro = 'nrs_bias_removal_gui', $
    ref_value = 'Stack', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Stack statistics', $
    uvalue = 'Stack statistics', event_pro = 'nrs_stack_statistics_gui', $
    ref_value = 'Stack', position = 'last', /separator

  envi_define_menu_button, buttonInfo, value = 'Aggregate stack', $
    uvalue = 'Aggregate stack', event_pro = 'nrs_aggregate_gui', $
    ref_value = 'Stack', position = 'last'

  envi_define_menu_button, buttoninfo, value = 'Bandwise aggregation', $
    uvalue = 'bandwise aggregation', event_pro = 'nrs_stack_aggregation_gui', $
    ref_value = 'Stack', position = 'last'

  envi_define_menu_button, buttonInfo, value = 'Stack layers', $
    uvalue = 'Stack layers', event_pro = 'nrs_stack_gui', $
    ref_value = 'Stack', position = 'last', /separator

  envi_define_menu_button, buttonInfo, value = 'Unstack layers', $
    uvalue = 'Unstack layers', event_pro = 'nrs_unstack_gui', $
    ref_value = 'Stack', position = 'last'

end
