pro nrs_climind_rnn_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_rnn_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climind_rnn_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_rnn_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_climind_rnn_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_climind_rnn_contentPanel = widget_base(uname = 'nrs_climind_rnn_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Count wet days by limit')

  nrs_climind_rnn_mainPanel = widget_base(nrs_climind_rnn_contentPanel, /frame, /col)

  nrs_climind_rnn_refstack = cw_dirfile(nrs_climind_rnn_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_climind_rnn_refstack' $
                , event_pro = 'nrs_climind_rnn_handle_input' $
              )

  nrs_climind_rnn_limit = fsc_inputfield(nrs_climind_rnn_mainPanel $
                , uname = 'nrs_climind_rnn_limit' $
                , title = 'Wet limit' $
                , /integervalue $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , value = 10 $
                , unittext = 'mm' $
              )
              
  nrs_climind_rnn_start_date = fsc_inputfield(nrs_climind_rnn_mainPanel $
                , uname = 'nrs_climind_rnn_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
              
  nrs_climind_rnn_end_date = fsc_inputfield(nrs_climind_rnn_mainPanel $
                , uname = 'nrs_climind_rnn_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
  
  nrs_climind_rnn_output_panel = widget_base(nrs_climind_rnn_contentPanel, /frame, /col)
  nrs_climind_rnn_outputFile = cw_dirfile(nrs_climind_rnn_output_panel, uname = 'nrs_climind_rnn_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_climind_rnn_contentPanel $
                , ok_uname = 'nrs_climind_rnn_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate rainfall period from timeseries' $
                , cancel_uname = 'nrs_climind_rnn_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climind_rnn_contentpanel

  ; Initialize stuff

  XManager, 'nrs_climind_rnn_gui', nrs_climind_rnn_contentPanel, /no_block
end
