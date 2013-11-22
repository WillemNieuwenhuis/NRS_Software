pro nrs_detect_burnt_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_detect_burnt_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_detect_burnt_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_detect_burnt_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_detect_burnt_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_detect_burnt_contentPanel = widget_base(uname = 'nrs_detect_burnt_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Detect burnt periods')

  nrs_detect_burnt_mainPanel = widget_base(nrs_detect_burnt_contentPanel, /frame, /col)

  nrs_detect_burnt_obs = cw_dirfile(nrs_detect_burnt_mainPanel $
                , title = 'Input timeseries table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_detect_burnt_obs' $
                , event_pro = 'nrs_detect_burnt_handle_input' $
              )

  nrs_detect_burnt_btn_group = cw_groupbox(nrs_detect_burnt_mainPanel, group_title = 'Parameters')
  nrs_detect_burnt_indices_base = widget_base(nrs_detect_burnt_btn_group, /nonexclusive, uname = 'nrs_detect_burnt_indices_base')
  nrs_detect_burnt_filter_noise = widget_button(nrs_detect_burnt_indices_base $
                , uname = 'nrs_detect_burnt_filter_noise' $
                , value = 'Apply noise filter' $
              )
  nrs_detect_burnt_use_thresh_column = widget_button(nrs_detect_burnt_indices_base $
                , uname = 'nrs_detect_burnt_use_thresh_column' $
                , value = 'Use lower threshold column' $
                , event_pro = 'nrs_detect_burnt_toggle_column' $
              )
  nrs_detect_burnt_avg_button = widget_button(nrs_detect_burnt_indices_base $
                , uname = 'nrs_detect_burnt_avg_button' $
                , value = 'Average threshold column' $
                , sensitiv = 0 $
              )

  nrs_detect_burnt_threshold = fsc_inputfield(nrs_detect_burnt_btn_group $
                , uname = 'nrs_detect_burnt_threshold' $
                , title = 'Threshold' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '0.9' $
                , xsize = text_small_width $
              )

  nrs_detect_burnt_output_panel = widget_base(nrs_detect_burnt_contentPanel, /frame, /col)
  nrs_detect_burnt_outputFile = cw_dirfile(nrs_detect_burnt_output_panel, uname = 'nrs_detect_burnt_outputFile' $
        , style = 'file' $
        , title = 'Output base name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_detect_burnt_contentPanel $
                , ok_uname = 'nrs_detect_burnt_gobutton', ok_value = 'Go!', ok_tooltip = 'Detect burnt periods' $
                , cancel_uname = 'nrs_detect_burnt_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_detect_burnt_contentpanel

  ; Initialize stuff

  ; show the dialog
  XManager, 'nrs_detect_burnt_gui', nrs_detect_burnt_contentPanel, /no_block
end
