pro nrs_aggregate_spectra_by_pol_gui_event, event
  compile_opt idl2

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_spectra_by_pol_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_aggregate_spectra_by_pol_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_spectra_by_pol_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_aggregate_spectra_by_pol_gui, event
  compile_opt idl2

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_aggregate_spectra_by_pol_contentPanel = widget_base(uname = 'nrs_aggregate_spectra_by_pol_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Aggregate spectrum')

  nrs_aggregate_spectra_by_pol_mainPanel = widget_base(nrs_aggregate_spectra_by_pol_contentPanel, /frame, /col)

  nrs_aggregate_spectra_by_pol_input_image = cw_dirfile(nrs_aggregate_spectra_by_pol_mainPanel $
    , title = 'Input spectral image' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_aggregate_spectra_by_pol_input_image' $
    , event_pro = 'nrs_aggregate_spectra_by_pol_handle_input' $
    )

  nrs_aggregate_spectra_by_pol_input = cw_dirfile(nrs_aggregate_spectra_by_pol_mainPanel $
    , title = 'Input polygons' $
    , style = 'file' $
    , filter = [['*.shp', '*.*'],['Shape files (*.shp)', 'All files (*.*)']] $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_aggregate_spectra_by_pol_input' $
    , event_pro = 'nrs_aggregate_spectra_by_pol_handle_shapefile' $
    )

  nrs_aggregate_spectra_by_pol_attr_panel = widget_base(nrs_aggregate_spectra_by_pol_mainPanel, /row)
  nrs_aggregate_spectra_by_pol_attr_label = widget_label(nrs_aggregate_spectra_by_pol_attr_panel $
    , value = 'Attribute field' $
    , xsize = label_width $
    )
  nrs_aggregate_spectra_by_pol_attr_combo = widget_combobox(nrs_aggregate_spectra_by_pol_attr_panel $
    , uname = 'nrs_aggregate_spectra_by_pol_attr_combo' $
    , xsize = label_wide_width $
    , value = [' '] $
    )

  nrs_aggregate_spectra_by_pol_aggr_func_panel = widget_base(nrs_aggregate_spectra_by_pol_mainPanel, /row)
  nrs_aggregate_spectra_by_pol_aggr_func_label = widget_label(nrs_aggregate_spectra_by_pol_aggr_func_panel $
    , value = 'Aggregation function' $
    , xsize = label_width $
    )
  nrs_aggregate_spectra_by_pol_aggr_func = widget_combobox(nrs_aggregate_spectra_by_pol_aggr_func_panel $
    , uname = 'nrs_aggregate_spectra_by_pol_aggr_func' $
    , value = ['Mean', 'Median', 'Min', 'Max'] $
    )

  nrs_aggregate_spectra_by_pol_output_panel = widget_base(nrs_aggregate_spectra_by_pol_contentPanel, /frame, /col)

  nrs_aggregate_spectra_by_pol_table_type = cw_bgroup(nrs_aggregate_spectra_by_pol_output_panel $
    , ['Spectrum as column', 'XY table'] $
    , /col, /exclusive $
    , set_value = 0 $
    , uname = 'nrs_aggregate_spectra_by_pol_table_type' $
    )

  nrs_aggregate_spectra_by_pol_outputFile = cw_dirfile(nrs_aggregate_spectra_by_pol_output_panel, uname = 'nrs_aggregate_spectra_by_pol_outputFile' $
    , style = 'file' $
    , title = 'Output table' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createButtonPanel, nrs_aggregate_spectra_by_pol_contentPanel $
    , ok_uname = 'nrs_aggregate_spectra_by_pol_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate aggregate spectra' $
    , cancel_uname = 'nrs_aggregate_spectra_by_pol_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_aggregate_spectra_by_pol_contentpanel

  ; Initialize stuff
;  widget_control, nrs_aggregate_spectra_by_pol_attr_combo, set_combobox_select = 0

  ; start dialog
  XManager, 'nrs_aggregate_spectra_by_pol_gui', nrs_aggregate_spectra_by_pol_contentPanel, /no_block
end
