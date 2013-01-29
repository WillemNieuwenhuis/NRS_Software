pro nrs_stack_compare_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_compare_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_compare_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_compare_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_stack_compare_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_stack_compare_contentPanel = widget_base(uname = 'nrs_stack_compare_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Compare two stacks')

  nrs_stack_compare_mainPanel = widget_base(nrs_stack_compare_contentPanel, /frame, /col)

  nrs_stack_compare_source_stack = cw_dirfile(nrs_stack_compare_mainPanel $
                , title = 'Source stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_compare_source_stack' $
                , event_pro = 'nrs_stack_compare_handle_input' $
              )

  nrs_stack_compare_target_stack = cw_dirfile(nrs_stack_compare_mainPanel $
                , title = 'Target stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_compare_target_stack' $
              )

  nrs_stack_compare_rep_options_base = widget_base(nrs_stack_compare_mainPanel, /nonexclusive $
                , uname = 'nrs_stack_compare_rep_options_base')
  nrs_stack_compare_bname_button = widget_button(nrs_stack_compare_rep_options_base $
                , uname = 'nrs_stack_compare_bname_button' $
                , value = 'Report band name differences' $
              )

  nrs_stack_compare_bvalue_button = widget_button(nrs_stack_compare_rep_options_base $
                , uname = 'nrs_stack_compare_bvalue_button' $
                , value = 'Report band value differences' $
              )

  nrs_stack_compare_output_panel = widget_base(nrs_stack_compare_contentPanel, /frame, /col)
  nrs_stack_compare_report_file = cw_dirfile(nrs_stack_compare_output_panel $
        , uname = 'nrs_stack_compare_report_file' $
        , style = 'file' $
        , title = 'Report file' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_stack_compare_contentPanel $
                , ok_uname = 'nrs_stack_compare_gobutton', ok_value = 'Go!', ok_tooltip = 'Compare stacks' $
                , cancel_uname = 'nrs_stack_compare_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_compare_contentpanel

  ; Initialize stuff

  XManager, 'nrs_stack_compare_gui', nrs_stack_compare_contentPanel, /no_block
end
