pro nrs_climind_rx5day_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Maximum consecutive 5-day precipitation', 'nrs_climind_rx5day_gui', PATH='Climate indices'
end

pro nrs_climind_rx5day_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_rx5day_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climind_rx5day_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_rx5day_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_climind_rx5day_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_climind_rx5day_contentPanel = widget_base(uname = 'nrs_climind_rx5day_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate RX5day climate index')

  nrs_climind_rx5day_mainPanel = widget_base(nrs_climind_rx5day_contentPanel, /frame, /col)

  nrs_climind_rx5day_refstack = cw_dirfile(nrs_climind_rx5day_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_climind_rx5day_refstack' $
                , event_pro = 'nrs_climind_rx5day_handle_input' $
              )

  nrs_climind_rx5day_start_date = fsc_inputfield(nrs_climind_rx5day_mainPanel $
                , uname = 'nrs_climind_rx5day_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
              
  nrs_climind_rx5day_end_date = fsc_inputfield(nrs_climind_rx5day_mainPanel $
                , uname = 'nrs_climind_rx5day_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
  
  nrs_climind_rx5day_period_panel = widget_base(nrs_climind_rx5day_mainPanel, /row)
  nrs_climind_rx5day_period_label = widget_label(nrs_climind_rx5day_period_panel, value = 'Aggregation period')
  nrs_climind_rx5day_period = cw_bgroup(nrs_climind_rx5day_period_panel $
                , ['Monthly', 'Annual'] $
                , /row, /exclusive $
                , set_value = 0 $
                , uname = 'nrs_climind_rx5day_period' $
              ) 

  nrs_climind_rx5day_output_panel = widget_base(nrs_climind_rx5day_contentPanel, /frame, /col)
  nrs_climind_rx5day_outputFile = cw_dirfile(nrs_climind_rx5day_output_panel, uname = 'nrs_climind_rx5day_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_climind_rx5day_contentPanel $
                , ok_uname = 'nrs_climind_rx5day_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate rainfall period from timeseries' $
                , cancel_uname = 'nrs_climind_rx5day_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climind_rx5day_contentpanel

  ; Initialize stuff

  XManager, 'nrs_climind_rx5day_gui', nrs_climind_rx5day_contentPanel, /no_block
end
