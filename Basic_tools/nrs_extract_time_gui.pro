pro nrs_extract_time_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_extract_time_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_extract_time_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_extract_time_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_extract_time_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_extract_time_contentPanel = widget_base(uname = 'nrs_extract_time_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Extract by time')

  nrs_extract_time_mainPanel = widget_base(nrs_extract_time_contentPanel, /frame, /col)

  nrs_extract_time_points = cw_dirfile(nrs_extract_time_mainPanel $
                , title = 'Input locations' $
                , style = 'file' $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables (txt, csv)', 'All files']] $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_extract_time_points' $
                , event_pro = 'nrs_extract_time_handle_points' $
              )

  group = cw_groupbox(nrs_extract_time_mainPanel, group_title = 'Timeseries')              
  nrs_extract_time_refstack = cw_dirfile(group $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_extract_time_refstack' $
                , event_pro = 'nrs_extract_time_handle_input' $
              )

  nrs_extract_time_start_date = fsc_inputfield(group $
                , uname = 'nrs_extract_time_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_extract_time_end_date = fsc_inputfield(group $
                , uname = 'nrs_extract_time_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )
  
  nrs_extract_time_fieldname = fsc_inputfield(nrs_extract_time_mainPanel $
                , uname = 'nrs_extract_time_fieldname' $
                , title = 'Field name' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_width $
                , /all_events $
              )
  
  nrs_extract_time_output_panel = widget_base(nrs_extract_time_contentPanel, /frame, /col)
  nrs_extract_time_outputFile = cw_dirfile(nrs_extract_time_output_panel, uname = 'nrs_extract_time_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_extract_time_contentPanel $
                , ok_uname = 'nrs_extract_time_gobutton', ok_value = 'Go!', ok_tooltip = 'Extract by time' $
                , cancel_uname = 'nrs_extract_time_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_extract_time_contentpanel

  ; Initialize stuff

  XManager, 'nrs_extract_time_gui', nrs_extract_time_contentPanel, /no_block
end
