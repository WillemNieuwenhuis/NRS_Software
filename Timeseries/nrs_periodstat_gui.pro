pro nrs_periodstat_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_periodstat_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_periodstat_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_periodstat_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_periodstat_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_periodstat_contentPanel = widget_base(uname = 'nrs_periodstat_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Timeseries (grouped) aggregation')

  nrs_periodstat_mainPanel = widget_base(nrs_periodstat_contentPanel, /frame, /col)

  nrs_periodstat_group = cw_groupbox(nrs_periodstat_mainPanel, group_title = 'Timeseries')              

  nrs_periodstat_refstack = cw_dirfile(nrs_periodstat_group $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_periodstat_refstack' $
                , event_pro = 'nrs_periodstat_handle_input' $
              )

  nrs_periodstat_start_date = fsc_inputfield(nrs_periodstat_group $
                , uname = 'nrs_periodstat_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_periodstat_end_date = fsc_inputfield(nrs_periodstat_group $
                , uname = 'nrs_periodstat_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )
  
  nrs_periodstat_aggr_panel = widget_base(nrs_periodstat_mainPanel, /row)
  nrs_periodstat_aggr_label = widget_label(nrs_periodstat_aggr_panel $
                , value = 'Output date interval' $
                , xsize = label_width $
              )
  nrs_periodstat_aggr_combo = widget_combobox(nrs_periodstat_aggr_panel $
                , uname = 'nrs_periodstat_aggr_combo' $
                , value = ['Day', '8-Day', '16-Day', 'Month', 'Year'] $
              )

  nrs_periodstat_indices_panel = widget_base(nrs_periodstat_mainPanel, /row)
  nrs_periodstat_indices_label = widget_label(nrs_periodstat_indices_panel $
                , value = 'Aggregation function' $
                , xsize = label_width $
              )
  nrs_periodstat_indices_combo = widget_combobox(nrs_periodstat_indices_panel $
                , uname = 'nrs_periodstat_indices_combo' $
                , value = ['Sum', 'Mean', 'Minumum', 'Maximum', 'Median', 'Standard deviation', 'Mode'] $
              )

  nrs_periodstat_grouping_panel = widget_base(nrs_periodstat_indices_panel, /nonexclusive, uname = 'nrs_periodstat_grouping_base')
  nrs_periodstat_grouping_button = widget_button(nrs_periodstat_grouping_panel $
                , uname = 'nrs_periodstat_grouping_button' $
                , value = 'Group (per year)' $
              )

  nrs_periodstat_output_panel = widget_base(nrs_periodstat_contentPanel, /frame, /col)
  nrs_periodstat_outputFile = cw_dirfile(nrs_periodstat_output_panel, uname = 'nrs_periodstat_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_periodstat_contentPanel $
                , ok_uname = 'nrs_periodstat_gobutton', ok_value = 'Go!', ok_tooltip = 'Harmonic analysis on timeseries' $
                , cancel_uname = 'nrs_periodstat_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_periodstat_contentpanel

  ; Initialize stuff

  XManager, 'nrs_periodstat_gui', nrs_periodstat_contentPanel, /no_block
end
