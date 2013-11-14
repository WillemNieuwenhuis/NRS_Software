pro nrs_climind_r95p_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_r95p_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climind_r95p_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_r95p_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_climind_r95p_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_climind_r95p_contentPanel = widget_base(uname = 'nrs_climind_r95p_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Annual precipitation on very wet days')

  nrs_climind_r95p_mainPanel = widget_base(nrs_climind_r95p_contentPanel, /frame, /col)

  nrs_climind_r95p_refstack = cw_dirfile(nrs_climind_r95p_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_climind_r95p_refstack' $
                , event_pro = 'nrs_climind_r95p_handle_input' $
              )

  nrs_climind_r95p_percentile = cw_dirfile(nrs_climind_r95p_mainPanel $
                , title = 'Percentile image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_climind_r95p_percentile' $
              )

  nrs_climind_r95p_start_date = fsc_inputfield(nrs_climind_r95p_mainPanel $
                , uname = 'nrs_climind_r95p_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
              
  nrs_climind_r95p_end_date = fsc_inputfield(nrs_climind_r95p_mainPanel $
                , uname = 'nrs_climind_r95p_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , unittext = '(dd-mm-yyyy)' $
              )
  
  nrs_climind_r95p_output_panel = widget_base(nrs_climind_r95p_contentPanel, /frame, /col)
  nrs_climind_r95p_outputFile = cw_dirfile(nrs_climind_r95p_output_panel, uname = 'nrs_climind_r95p_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_climind_r95p_contentPanel $
                , ok_uname = 'nrs_climind_r95p_gobutton', ok_value = 'Go!', ok_tooltip = 'Annual precipitation on very wet days' $
                , cancel_uname = 'nrs_climind_r95p_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climind_r95p_contentpanel

  ; Initialize stuff

  XManager, 'nrs_climind_r95p_gui', nrs_climind_r95p_contentPanel, /no_block
end
