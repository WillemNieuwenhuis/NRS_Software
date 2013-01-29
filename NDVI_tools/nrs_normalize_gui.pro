pro nrs_normalize_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_normalize_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_normalize_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_normalize_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_normalize_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_normalize_contentPanel = widget_base(uname = 'nrs_normalize_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Normalize indices')

  nrs_normalize_mainPanel = widget_base(nrs_normalize_contentPanel, /frame, /col)

  nrs_normalize_input_image = cw_dirfile(nrs_normalize_mainPanel $
                , title = 'Input series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_normalize_input_image' $
                , event_pro = 'nrs_normalize_handle_input' $
              )

  nrs_normalize_period = cw_bgroup(nrs_normalize_mainPanel $
                , ['Normalize over entire series', 'Normalize per year'] $
                , /col, /exclusive $
                , set_value = 0 $
;                , /frame $
                , uname = 'nrs_normalize_period' $
              ) 

  nrs_normalize_use_perc_base = widget_base(nrs_normalize_mainPanel, /nonexclusive, uname = 'nrs_normalize_use_perc_base')
  nrs_normalize_use_perc_button = widget_button(nrs_normalize_use_perc_base $
                , uname = 'nrs_normalize_use_perc_button' $
                , value = 'Output in percentages' $
              )

  nrs_normalize_minmax = cw_bgroup(nrs_normalize_mainPanel $
                , ['Normalize over minimum', 'Normalize over maximum'] $
                , /col, /exclusive $
                , set_value = 0 $
;                , /frame $
                , uname = 'nrs_normalize_minmax' $
              ) 

  nrs_normalize_output_panel = widget_base(nrs_normalize_contentPanel, /frame, /col)
  nrs_normalize_outputFile = cw_dirfile(nrs_normalize_output_panel, uname = 'nrs_normalize_outputFile' $
        , style = 'file' $
        , title = 'Output series' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_normalize_contentPanel $
                , ok_uname = 'nrs_normalize_gobutton', ok_value = 'Go!', ok_tooltip = 'Normalize indices' $
                , cancel_uname = 'nrs_normalize_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_normalize_contentpanel

  ; Initialize stuff
  widget_control, nrs_normalize_use_perc_button, /set_button

  XManager, 'nrs_normalize_gui', nrs_normalize_contentPanel, /no_block
end
