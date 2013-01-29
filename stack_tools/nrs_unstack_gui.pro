pro nrs_unstack_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_unstack_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_unstack_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_unstack_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_unstack_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_unstack_contentPanel = widget_base(uname = 'nrs_unstack_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Unstack image stack')

  nrs_unstack_mainPanel = widget_base(nrs_unstack_contentPanel, /frame, /col)

  nrs_unstack_refstack = cw_dirfile(nrs_unstack_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_unstack_refstack' $
                , event_pro = 'nrs_unstack_handle_input' $
              )

  nrs_unstack_output_panel = widget_base(nrs_unstack_contentPanel, /frame, /col)
  nrs_unstack_outputFile = cw_dirfile(nrs_unstack_output_panel, uname = 'nrs_unstack_outputFile' $
        , style = 'file' $
        , title = 'Output basename' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_unstack_contentPanel $
                , ok_uname = 'nrs_unstack_gobutton', ok_value = 'Go!', ok_tooltip = 'Extract layers from stack to separate files' $
                , cancel_uname = 'nrs_unstack_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_unstack_contentpanel

  ; Initialize stuff

  XManager, 'nrs_unstack_gui', nrs_unstack_contentPanel, /no_block
end
