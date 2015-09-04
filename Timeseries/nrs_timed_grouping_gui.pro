pro nrs_timed_grouping_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_timed_grouping_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_timed_grouping_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_timed_grouping_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_timed_grouping_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_timed_grouping_contentPanel = widget_base(uname = 'nrs_timed_grouping_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Timeseries aggregation')

  nrs_timed_grouping_mainPanel = widget_base(nrs_timed_grouping_contentPanel, /frame, /col)

  nrs_timed_grouping_group = cw_groupbox(nrs_timed_grouping_mainPanel, group_title = 'Timeseries')              

  nrs_timed_grouping_refstack = cw_dirfile(nrs_timed_grouping_group $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_timed_grouping_refstack' $
                , event_pro = 'nrs_timed_grouping_handle_input' $
              )

  nrs_timed_grouping_start_date = fsc_inputfield(nrs_timed_grouping_group $
                , uname = 'nrs_timed_grouping_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy [hh:mm:ss])' $
                , value = '' $
                , xsize = text_small_width * 2 $
                , /all_events $
                , event_pro = 'nrs_timed_grouping_handle_input' $
              )

  nrs_timed_grouping_end_date = fsc_inputfield(nrs_timed_grouping_group $
                , uname = 'nrs_timed_grouping_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy [hh:mm:ss])' $
                , value = '' $
                , xsize = text_small_width * 2 $
                , /all_events $
                , event_pro = 'nrs_timed_grouping_handle_input' $
              )

  void = widget_base(nrs_timed_grouping_group, /row)
  nrs_period_aggr_input_period_label = widget_label(void $
                , uname = 'nrs_period_aggr_input_period_label' $
                , xsize = label_wide_width $
                , value = 'Input period: --' $
              )
  
  nrs_timed_grouping_param_group = cw_groupbox(nrs_timed_grouping_mainPanel, group_title = 'Parameters')              
  
  dummy = widget_label(nrs_timed_grouping_param_group, value = 'Grouping', /align_left)
  nrs_timed_grouping_grouping = widget_base(nrs_timed_grouping_param_group, /col)
  
  nrs_timed_grouping_lvl1_panel = widget_base(nrs_timed_grouping_grouping, /row)
  
  nrs_timed_grouping_level1 = widget_base(nrs_timed_grouping_lvl1_panel, /row, /nonexclusive)
  nrs_timed_grouping_lvl1_button = widget_button(nrs_timed_grouping_level1 $
                , uname = 'nrs_timed_grouping_lvl1_button' $
                , value = 'Level 1' $
                , xsize = label_width $
                , event_pro = 'nrs_timed_grouping_toggle_levels' $
;                , sensitiv = 0 $
              )

  nrs_timed_grouping_level1_combo = widget_combobox(nrs_timed_grouping_lvl1_panel $
                , uname = 'nrs_timed_grouping_level1_combo' $
                , value = ['Day', '8-Day', '10-Day', '16-Day', 'Month', 'Year'] $
                , sensitiv = 0 $
              )

  nrs_timed_grouping_lvl2_panel = widget_base(nrs_timed_grouping_grouping, /row)
  nrs_timed_grouping_level2 = widget_base(nrs_timed_grouping_lvl2_panel, /row, /nonexclusive)
  nrs_timed_grouping_lvl2_button = widget_button(nrs_timed_grouping_level2 $
                , uname = 'nrs_timed_grouping_lvl2_button' $
                , value = 'Level 2' $
                , xsize = label_width $
                , event_pro = 'nrs_timed_grouping_toggle_levels' $
                , sensitiv = 0 $
              )

  nrs_timed_grouping_level2_combo = widget_combobox(nrs_timed_grouping_lvl2_panel $
                , uname = 'nrs_timed_grouping_level2_combo' $
                , value = ['Year'] $
                , sensitiv = 0 $
              )

  nrs_timed_grouping_indices_panel = widget_base(nrs_timed_grouping_param_group, /row)
  nrs_timed_grouping_indices_label = widget_label(nrs_timed_grouping_indices_panel $
                , value = 'Aggregation function' $
                , xsize = label_width $
              )

  nrs_timed_grouping_indices_combo = widget_combobox(nrs_timed_grouping_indices_panel $
                , uname = 'nrs_timed_grouping_indices_combo' $
                , value = ['Sum', 'Mean', 'Minumum', 'Maximum', 'Median'] $ ;, 'Standard deviation', 'Mode'] $
              )

  nrs_timed_grouping_output_panel = widget_base(nrs_timed_grouping_contentPanel, /frame, /col)
  nrs_timed_grouping_outputFile = cw_dirfile(nrs_timed_grouping_output_panel, uname = 'nrs_timed_grouping_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_timed_grouping_contentPanel $
                , ok_uname = 'nrs_timed_grouping_gobutton', ok_value = 'Go!', ok_tooltip = 'Harmonic analysis on timeseries' $
                , cancel_uname = 'nrs_timed_grouping_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_timed_grouping_contentpanel

  ; Initialize stuff
  

  ; start the form
  XManager, 'nrs_timed_grouping_gui', nrs_timed_grouping_contentPanel, /no_block
end
