pro nrs_aggregate_spectrum_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Aggregate spectrum', 'nrs_aggregate_spectrum_gui', PATH='Spectral'
end

pro nrs_aggregate_spectrum_gui_event, event
  compile_opt idl2
  
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_spectrum_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_aggregate_spectrum_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_spectrum_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_aggregate_spectrum_gui, event
  compile_opt idl2
  
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_aggregate_spectrum_contentPanel = widget_base(uname = 'nrs_aggregate_spectrum_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Aggregate spectrum')

  nrs_aggregate_spectrum_mainPanel = widget_base(nrs_aggregate_spectrum_contentPanel, /frame, /col)

  nrs_aggregate_spectrum_input_image = cw_dirfile(nrs_aggregate_spectrum_mainPanel $
                , title = 'Input spectral image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_aggregate_spectrum_input_image' $
                , event_pro = 'nrs_aggregate_spectrum_handle_input' $
              )

  nrs_aggregate_spectrum_input_table = cw_dirfile(nrs_aggregate_spectrum_mainPanel $
                , title = 'Input locations' $
                , style = 'file' $
                , filter = [['*.csv', '*.shp', '*.*'],['CSV files (*.csv)', 'Shape files (*.shp)', 'All files (*.*)']] $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_aggregate_spectrum_input_table' $
              )

  nrs_aggregate_spectrum_aggr_func_panel = widget_base(nrs_aggregate_spectrum_mainPanel, /row)
  nrs_aggregate_spectrum_aggr_func_label = widget_label(nrs_aggregate_spectrum_aggr_func_panel $
                , value = 'Aggregation function' $
                , xsize = label_width $
              )
  nrs_aggregate_spectrum_aggr_func = widget_combobox(nrs_aggregate_spectrum_aggr_func_panel $
                , uname = 'nrs_aggregate_spectrum_aggr_func' $
                , value = ['Mean', 'Min', 'Max'] $
              )

  nrs_aggregate_spectrum_kernel_panel = widget_base(nrs_aggregate_spectrum_mainPanel, /row)
  nrs_aggregate_spectrum_kernel_label = widget_label(nrs_aggregate_spectrum_kernel_panel $
                , value = 'Kernel size' $
                , xsize = label_width $
              )
  nrs_aggregate_spectrum_kernel_combo = widget_combobox(nrs_aggregate_spectrum_kernel_panel $
                , uname = 'nrs_aggregate_spectrum_kernel_combo' $
                , value = ['1', '3', '5', '7', '9', '11'] $
              )

  nrs_aggregate_spectrum_output_panel = widget_base(nrs_aggregate_spectrum_contentPanel, /frame, /col)
  nrs_aggregate_spectrum_outputFile = cw_dirfile(nrs_aggregate_spectrum_output_panel, uname = 'nrs_aggregate_spectrum_outputFile' $
        , style = 'file' $
        , title = 'Output table' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_aggregate_spectrum_contentPanel $
                , ok_uname = 'nrs_aggregate_spectrum_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate aggregate spectra' $
                , cancel_uname = 'nrs_aggregate_spectrum_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_aggregate_spectrum_contentpanel

  ; Initialize stuff
  widget_control, nrs_aggregate_spectrum_kernel_combo, set_combobox_select = 2 

  XManager, 'nrs_aggregate_spectrum_gui', nrs_aggregate_spectrum_contentPanel, /no_block
end
