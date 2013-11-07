pro nrs_growth_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_growth_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_growth_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_growth_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_growth_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_growth_contentPanel = widget_base(uname = 'nrs_growth_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Find start of growing season')

  nrs_growth_mainPanel = widget_base(nrs_growth_contentPanel, /frame, /col)

  nrs_growth_refstack = cw_dirfile(nrs_growth_mainPanel $
                , title = 'Input NDVI' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_growth_refstack' $
                , event_pro = 'nrs_growth_handle_input' $
              )

  nrs_growth_output_panel = widget_base(nrs_growth_contentPanel, /frame, /col)
  nrs_growth_outputFile = cw_dirfile(nrs_growth_output_panel, uname = 'nrs_growth_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_growth_contentPanel $
                , ok_uname = 'nrs_growth_gobutton', ok_value = 'Go!', ok_tooltip = 'Find start of growing season from NDVI' $
                , cancel_uname = 'nrs_growth_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_growth_contentpanel

  ; Initialize stuff

  XManager, 'nrs_growth_gui', nrs_growth_contentPanel, /no_block
end
