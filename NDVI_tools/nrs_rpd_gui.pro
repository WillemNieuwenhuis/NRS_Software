pro nrs_rpd_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rpd_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_rpd_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rpd_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_rpd_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_rpd_contentPanel = widget_base(uname = 'nrs_rpd_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate RPD')

  nrs_rpd_mainPanel = widget_base(nrs_rpd_contentPanel, /frame, /col)

  nrs_rpd_refstack = cw_dirfile(nrs_rpd_mainPanel $
                , title = 'Input NDVI' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_rpd_refstack' $
                , event_pro = 'nrs_rpd_handle_input' $
              )

  nrs_rpd_output_panel = widget_base(nrs_rpd_contentPanel, /frame, /col)
  nrs_rpd_outputFile = cw_dirfile(nrs_rpd_output_panel, uname = 'nrs_rpd_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_rpd_contentPanel $
                , ok_uname = 'nrs_rpd_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate RPD values from NDVI' $
                , cancel_uname = 'nrs_rpd_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_rpd_contentpanel

  ; Initialize stuff

  XManager, 'nrs_rpd_gui', nrs_rpd_contentPanel, /no_block
end
