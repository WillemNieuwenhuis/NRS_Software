pro nrs_payment_averages_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_payment_averages_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_payment_averages_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_payment_averages_handlego, Event
    end
    else:

  endcase

end

pro nrs_payment_averages_gui, event, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0) $
  }

  label_width = 180
  label_wide_width = 150
  text_width =  70
  text_small_width = 10
  num_width =   15

  nrs_payment_averages_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'nrs_payment_averages_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Determine payment averages')

  nrs_payment_averages_mainPanel = widget_base(nrs_payment_averages_contentPanel, /frame, /col)

  nrs_payment_averages_inputPanel = cw_groupbox(nrs_payment_averages_mainPanel, group_title = 'Timeseries')

  nrs_payment_averages_refimage = cw_dirfile(nrs_payment_averages_inputPanel $
    , title = 'NDVI timeseries' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_refimage' $
    , event_pro = 'nrs_payment_averages_handle_input' $
    )

  nrs_payment_averages_classes = cw_dirfile(nrs_payment_averages_inputPanel $
    , title = 'Classified NDVI image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_classes' $
    )

  nrs_payment_averages_early_season_panel = widget_base(nrs_payment_averages_inputPanel, col=2, xpad = 0)
  nrs_payment_averages_early_season = widget_base(nrs_payment_averages_early_season_panel, /nonexclusive, uname = 'nrs_payment_averages_early_season', xsize=label_width)
  nrs_payment_averages_early_season_button = widget_button(nrs_payment_averages_early_season $
    , uname = 'nrs_payment_averages_early_season_button' $
    , value = 'Early growing season' $
    , event_pro = 'nrs_payment_averages_early_season_toggle' $
    )

  nrs_payment_averages_season_early_table = cw_dirfile(nrs_payment_averages_early_season_panel $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_season_early_table' $
    )

  nrs_payment_averages_late_season_panel = widget_base(nrs_payment_averages_inputPanel, col=2, xpad = 0)
  nrs_payment_averages_late_season = widget_base(nrs_payment_averages_late_season_panel, /nonexclusive, uname = 'nrs_payment_averages_late_season', xsize=label_width)
  nrs_payment_averages_late_season_button = widget_button(nrs_payment_averages_late_season $
    , uname = 'nrs_payment_averages_late_season_button' $
    , value = 'Late growing season' $
    , event_pro = 'nrs_payment_averages_late_season_toggle' $
    )

  nrs_payment_averages_season_late_table = cw_dirfile(nrs_payment_averages_late_season_panel $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_season_late_table' $
    )

  nrs_payment_averages_long_season_panel = widget_base(nrs_payment_averages_inputPanel, col=2, xpad = 0)
  nrs_payment_averages_long_season = widget_base(nrs_payment_averages_long_season_panel, /nonexclusive, uname = 'nrs_payment_averages_long_season', xsize=label_width)
  nrs_payment_averages_long_season_button = widget_button(nrs_payment_averages_long_season $
    , uname = 'nrs_payment_averages_long_season_button' $
    , value = 'long growing season' $
    , event_pro = 'nrs_payment_averages_long_season_toggle' $
    )

  nrs_payment_averages_season_long_table = cw_dirfile(nrs_payment_averages_long_season_panel $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_season_long_table' $
    )

  nrs_payment_averages_season_p5table = cw_dirfile(nrs_payment_averages_inputPanel $
    , title = 'Low percentile table (exit)' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_season_p5table' $
    )

  nrs_payment_averages_season_p25table = cw_dirfile(nrs_payment_averages_inputPanel $
    , title = 'High percentile table (trigger)' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_payment_averages_season_p25table' $
    )

  nrs_payment_averages_start_date = fsc_inputfield(nrs_payment_averages_inputPanel $
    , uname = 'nrs_payment_averages_start_date' $
    , title = 'Start date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )

  nrs_payment_averages_end_date = fsc_inputfield(nrs_payment_averages_inputPanel $
    , uname = 'nrs_payment_averages_end_date' $
    , title = 'End date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )

  group = cw_groupbox(nrs_payment_averages_mainPanel, group_title = 'Parameters')
  nrs_payment_averages_imgperyear = fsc_inputfield(group $
    , uname = 'nrs_payment_averages_imgperyear' $
    , title = 'NDVI layers per year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '36' $
    , xsize = text_small_width $
    , /integervalue $
    , /all_events $
    )

  nrs_payment_averages_outputPanel = widget_base(nrs_payment_averages_contentPanel, /frame, /col)
  frame_offset = 13

  nrs_payment_averages_grow_season_output = widget_base(nrs_payment_averages_outputPanel, /nonexclusive, uname = 'nrs_payment_averages_grow_season_output', xsize=label_width + frame_offset)
  nrs_payment_averages_grow_season_button = widget_button(nrs_payment_averages_grow_season_output $
    , uname = 'nrs_payment_averages_grow_season_button' $
    , value = 'Retain growing season result' $
    )

  nrs_payment_averages_percentages_output = widget_base(nrs_payment_averages_outputPanel, /nonexclusive, uname = 'nrs_payment_averages_percentages_output', xsize=label_width + frame_offset)
  nrs_payment_averages_percentages_button = widget_button(nrs_payment_averages_percentages_output $
    , uname = 'nrs_payment_averages_percentages_button' $
    , value = 'Retain payment percentages result' $
    )

  nrs_payment_averages_output = cw_dirfile(nrs_payment_averages_outputPanel $
    , title = 'Payment averages' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width + frame_offset $
    , uname = 'nrs_payment_averages_output' $
    )

  nrs_gui_createbuttonpanel, nrs_payment_averages_contentPanel $
    , ok_uname = 'nrs_payment_averages_gobutton', ok_value = 'Calculate', ok_tooltip = 'Determine pay percentage' $
    , cancel_uname = 'nrs_payment_averages_cancelbutton', cancel_value = 'Cancel', cancel_tooltip = 'Cancel the operation'


  ; Make sure we create the form
  widget_control, /REALIZE, nrs_payment_averages_contentPanel

  ; Initialize stuff
  ;  widget_control, nrs_payment_averages_absdiff, /set_button
  state.parent = nrs_payment_averages_contentPanel
  widget_control, nrs_payment_averages_contentPanel, set_uvalue = state
  widget_control, nrs_payment_averages_season_early_table, sensitiv = 0
  widget_control, nrs_payment_averages_season_late_table, sensitiv = 0
  widget_control, nrs_payment_averages_season_long_table, sensitiv = 0

  xmanager, 'nrs_payment_averages_gui', nrs_payment_averages_contentPanel, /NO_BLOCK
end

