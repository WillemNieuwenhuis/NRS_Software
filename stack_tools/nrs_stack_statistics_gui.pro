pro nrs_stack_statistics_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_statistics_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_statistics_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_statistics_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_stack_statistics_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_stack_statistics_contentPanel = widget_base(uname = 'nrs_stack_statistics_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate statistics')

  nrs_stack_statistics_mainPanel = widget_base(nrs_stack_statistics_contentPanel, /frame, /col)

  nrs_stack_statistics_refstack = cw_dirfile(nrs_stack_statistics_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_statistics_refstack' $
                , event_pro = 'nrs_stack_statistics_handle_input' $
              )

  nrs_stack_statistics_ignore = fsc_inputfield(nrs_stack_statistics_mainPanel $
    , uname = 'nrs_stack_statistics_ignore' $
    , title = 'Ignore value' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_stack_statistics_output_panel = widget_base(nrs_stack_statistics_contentPanel, /frame, /col)
  nrs_stack_statistics_outputFile = cw_dirfile(nrs_stack_statistics_output_panel, uname = 'nrs_stack_statistics_outputFile' $
        , style = 'file' $
        , title = 'Output statistics' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_stack_statistics_contentPanel $
                , ok_uname = 'nrs_stack_statistics_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate basic statistics' $
                , cancel_uname = 'nrs_stack_statistics_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_statistics_contentpanel

  ; Initialize stuff

  XManager, 'nrs_stack_statistics_gui', nrs_stack_statistics_contentPanel, /no_block
end
