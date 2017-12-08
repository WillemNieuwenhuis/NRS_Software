pro nrs_climind_table_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_table_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climind_table_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_table_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_climind_table_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_climind_table_contentPanel = widget_base(uname = 'nrs_climind_table_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Precipitation indices')

  nrs_climind_table_mainPanel = widget_base(nrs_climind_table_contentPanel, /frame, /col)

  nrs_climind_table_obs = cw_dirfile(nrs_climind_table_mainPanel $
    , title = 'Precipitation table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_climind_table_obs' $
    )

  nrs_climind_table_history_option = cw_bgroup(nrs_climind_table_mainPanel $
    , ['Historical data from input', 'Separate historical table'] $
    , /col, /exclusive $
    , set_value = 0 $
    , uname = 'nrs_climind_table_history_option' $
    , event_funct = 'nrs_climind_table_history_select' $
    )

  nrs_climind_table_basedates_base = widget_base(nrs_climind_table_mainPanel, /col, uname = 'nrs_climind_table_basedates_base')
  nrs_climind_table_base_startdate = fsc_inputfield(nrs_climind_table_basedates_base $
    , uname = 'nrs_climind_table_base_startdate' $
    , title = 'Start year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = num_width $
    , /all_events $
    )

  nrs_climind_table_base_enddate = fsc_inputfield(nrs_climind_table_basedates_base $
    , uname = 'nrs_climind_table_base_enddate' $
    , title = 'End year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = num_width $
    , /all_events $
    )

  nrs_climind_table_basetable_base = widget_base(nrs_climind_table_mainPanel, /col, uname = 'nrs_climind_table_basetable_base')
  nrs_climind_history_table = cw_dirfile(nrs_climind_table_basetable_base $
    , title = 'Historical table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_climind_history_table' $
    )

  nrs_climind_table_indices_base = widget_base(nrs_climind_table_mainPanel, /nonexclusive, uname = 'nrs_climind_table_indices_base')
  nrs_climind_table_rnn_button = widget_button(nrs_climind_table_indices_base $
    , uname = 'nrs_climind_table_rnn_button' $
    , value = 'Rnn' $
    , event_pro = 'nrs_climind_table_rnn_toggle' $
    )

  nrs_climind_table_rnn_limit = fsc_inputfield(nrs_climind_table_mainPanel $
    , uname = 'nrs_climind_table_rnn_limit' $
    , title = 'Limit' $
    , unit = 'mm' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '10' $
    , xsize = num_width $
    , /all_events $
    )

  nrs_climind_table_indices_base2 = widget_base(nrs_climind_table_mainPanel, /nonexclusive, uname = 'nrs_climind_table_indices_base2')
  nrs_climind_table_rx5_button = widget_button(nrs_climind_table_indices_base2 $
    , uname = 'nrs_climind_table_rx5_button' $
    , value = 'Rx1 / Rx5' $
    )

  nrs_climind_table_r95ptot_button = widget_button(nrs_climind_table_indices_base2 $
    , uname = 'nrs_climind_table_r95ptot_button' $
    , value = 'R95ptot' $
    )

  nrs_climind_table_prcptot_button = widget_button(nrs_climind_table_indices_base2 $
    , uname = 'nrs_climind_table_prcptot_button' $
    , value = 'Prcptot' $
    )

  nrs_climind_table_output_panel = widget_base(nrs_climind_table_contentPanel, /frame, /col)
  nrs_climind_table_outputFile = cw_dirfile(nrs_climind_table_output_panel, uname = 'nrs_climind_table_outputFile' $
    , style = 'file' $
    , title = 'Output basename' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createButtonPanel, nrs_climind_table_contentPanel $
    , ok_uname = 'nrs_climind_table_gobutton', ok_tooltip = 'Combine separate layers into a single stack' $
    , cancel_uname = 'nrs_climind_table_cancelbutton', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climind_table_contentpanel

  ; Initialize stuff
  nrs_climind_table_rnn_limit->SetProperty, sensitive = 0
  widget_control, nrs_climind_table_basedates_base, sensitive = 1
  widget_control, nrs_climind_table_basetable_base, sensitive = 0

  XManager, 'nrs_climind_table_gui', nrs_climind_table_contentPanel, /no_block
end
