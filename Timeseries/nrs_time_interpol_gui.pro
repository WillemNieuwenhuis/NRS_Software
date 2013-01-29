pro nrs_time_interpol_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_time_interpol_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_time_interpol_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_time_interpol_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_time_interpol_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_time_interpol_contentPanel = widget_base(uname = 'nrs_time_interpol_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Timeseries interpolation')

  nrs_time_interpol_mainPanel = widget_base(nrs_time_interpol_contentPanel, /frame, /col)

  nrs_time_interpol_group = cw_groupbox(nrs_time_interpol_mainPanel, group_title = 'Timeseries')              

  nrs_time_interpol_refstack = cw_dirfile(nrs_time_interpol_group $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_time_interpol_refstack' $
                , event_pro = 'nrs_time_interpol_handle_input' $
              )

  nrs_time_interpol_start_date = fsc_inputfield(nrs_time_interpol_group $
                , uname = 'nrs_time_interpol_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_time_interpol_end_date = fsc_inputfield(nrs_time_interpol_group $
                , uname = 'nrs_time_interpol_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )
  
  nrs_time_interpol_period_panel = widget_base(nrs_time_interpol_mainPanel, /row)
  nrs_time_interpol_period_label = widget_label(nrs_time_interpol_period_panel $
                , value = 'Output date interval' $
                , xsize = label_width $
              )
  nrs_time_interpol_period_combo = widget_combobox(nrs_time_interpol_period_panel $
                , uname = 'nrs_time_interpol_period_combo' $
                , value = ['Day', '8-Day', '10-Day', '16-Day', 'Month', 'Year'] $
              )

  nrs_time_interpol_output_panel = widget_base(nrs_time_interpol_contentPanel, /frame, /col)
  nrs_time_interpol_outputFile = cw_dirfile(nrs_time_interpol_output_panel, uname = 'nrs_time_interpol_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_time_interpol_contentPanel $
                , ok_uname = 'nrs_time_interpol_gobutton', ok_value = 'Go!', ok_tooltip = 'Interpolation on timeseries' $
                , cancel_uname = 'nrs_time_interpol_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_time_interpol_contentpanel

  ; Initialize stuff

  XManager, 'nrs_time_interpol_gui', nrs_time_interpol_contentPanel, /no_block
end
