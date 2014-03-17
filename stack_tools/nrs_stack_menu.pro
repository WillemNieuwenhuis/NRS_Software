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

  envi_define_menu_button, buttonInfo, value = 'Stack quartiles', $
    uvalue = 'Stack quartiles', event_pro = 'nrs_stack_quartiles_gui', $
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

  envi_define_menu_button, buttonInfo, value = 'Reverse layers', $
    uvalue = 'Reverse layers', event_pro = 'nrs_stack_reverse_gui', $
    ref_value = 'Stack', position = 'last'
end

; Also add as ENVI 5 extensions
pro nrs_stack_tools_extensions_init
  compile_opt idl2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Compare stacks', 'nrs_stack_compare_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Remove bias', 'nrs_bias_removal_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Stack statistics', 'nrs_stack_statistics_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Stack quartiles', 'nrs_stack_quartiles_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Aggregate stack', 'nrs_aggregate_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Bandwise aggregation', 'nrs_stack_aggregation_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Stack layers', 'nrs_stack_gui', PATH='NRS/Stack tools'
  e.AddExtension, 'Unstack layers', 'nrs_unstack_gui', PATH='NRS/Stack tools'
  e.addextension, 'Reverse layers', 'nrs_stack_reverse_gui', PATH='NRS/Stack tools'
end

; ENVI 5 compatibility
pro nrs_stack_tools
  compile_opt idl2
end
