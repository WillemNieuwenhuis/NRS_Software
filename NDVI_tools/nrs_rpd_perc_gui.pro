pro nrs_rpd_perc_gui_event, event
  compile_opt idl2

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rpd_perc_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_rpd_perc_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rpd_perc_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_rpd_perc_gui, event
  compile_opt idl2

  label_width = 110
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_rpd_perc_contentPanel = widget_base(uname = 'nrs_rpd_perc_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Locate first threshold crossing')

  nrs_rpd_perc_mainPanel = widget_base(nrs_rpd_perc_contentPanel, /frame, /col)

  nrs_rpd_perc_refstack = cw_dirfile(nrs_rpd_perc_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_rpd_perc_refstack' $
              )

  nrs_rpd_perc_locations = cw_dirfile(nrs_rpd_perc_mainPanel $
                , title = 'Location table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_rpd_perc_locations' $
                , event_pro = 'nrs_rpd_perc_handle_input' $
              )

  nrs_rpd_perc_percentage = fsc_inputfield(nrs_rpd_perc_mainPanel $
                , uname = 'nrs_rpd_perc_percentage' $
                , title = 'Threshold percentage' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '50' $
                , /integer $
                , unittext = ' %' $
                , xsize = text_small_width $
              )

  nrs_rpd_perc_window_base = widget_base(nrs_rpd_perc_mainPanel, /nonexclusive, uname = 'nrs_rpd_perc_window_base')
  nrs_rpd_perc_window_button = widget_button(nrs_rpd_perc_window_base $
                , uname = 'nrs_rpd_perc_window_button' $
                , value = 'Use window' $
                , event_pro = 'nrs_rpd_perc_toggle_window' $
              )

  nrs_rpd_perc_wbase = widget_base(nrs_rpd_perc_mainPanel, /col, xpad = 10, sensitiv = 0 $
                , uname = 'nrs_rpd_perc_wbase' $
              )
  nrs_rpd_perc_window = fsc_inputfield(nrs_rpd_perc_wbase $
                , uname = 'nrs_rpd_perc_window' $
                , title = 'Windows side' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '3' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_rpd_perc_valid_base = widget_base(nrs_rpd_perc_mainPanel, /nonexclusive, uname = 'nrs_rpd_perc_valid_base')
  nrs_rpd_perc_valid_button = widget_button(nrs_rpd_perc_valid_base $
                , uname = 'nrs_rpd_perc_valid_button' $
                , value = 'Exclude winter' $
              )

  nrs_rpd_perc_output_panel = widget_base(nrs_rpd_perc_contentPanel, /frame, /col)
  nrs_rpd_perc_outputFile = cw_dirfile(nrs_rpd_perc_output_panel, uname = 'nrs_rpd_perc_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_rpd_perc_contentPanel $
                , ok_uname = 'nrs_rpd_perc_gobutton', ok_value = 'Go!', ok_tooltip = 'Find first threshold crossing' $
                , cancel_uname = 'nrs_rpd_perc_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_rpd_perc_contentpanel

  ; Initialize stuff

  XManager, 'nrs_rpd_perc_gui', nrs_rpd_perc_contentPanel, /no_block
end
