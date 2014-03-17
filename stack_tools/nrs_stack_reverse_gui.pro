pro nrs_stack_reverse_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  
  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_stack_reverse_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_reverse_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_stack_reverse_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase
  
end

pro nrs_stack_reverse_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15
  
  nrs_stack_reverse_contentPanel = widget_base(uname = 'nrs_stack_reverse_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Reverse band order')
    
  nrs_stack_reverse_mainPanel = widget_base(nrs_stack_reverse_contentPanel, /frame, /col)
  
  nrs_stack_reverse_refstack = cw_dirfile(nrs_stack_reverse_mainPanel $
    , title = 'Input stack' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_stack_reverse_refstack' $
    , event_pro = 'nrs_stack_reverse_handle_input' $
    )
    
  nrs_stack_reverse_options_base = widget_base(nrs_stack_reverse_mainPanel, /nonexclusive $
    , uname = 'nrs_stack_reverse_options_base')
  nrs_stack_reverse_bname_button = widget_button(nrs_stack_reverse_options_base $
    , uname = 'nrs_stack_reverse_bname_button' $
    , value = 'Keep band names with bands' $
    )
    
  nrs_stack_reverse_output_panel = widget_base(nrs_stack_reverse_contentPanel, /frame, /col)
  nrs_stack_reverse_outputFile = cw_dirfile(nrs_stack_reverse_output_panel, uname = 'nrs_stack_reverse_outputFile' $
    , style = 'file' $
    , title = 'Output stack' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )
    
  nrs_gui_createbuttonpanel, nrs_stack_reverse_contentPanel $
    , ok_uname = 'nrs_stack_reverse_gobutton', ok_value = 'Go!', ok_tooltip = 'Reverse band order' $
    , cancel_uname = 'nrs_stack_reverse_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'
    
  ; Make sure we create the form
  widget_control, /realize, nrs_stack_reverse_contentpanel
  
  ; Initialize stuff
  
  xmanager, 'nrs_stack_reverse_gui', nrs_stack_reverse_contentPanel, /no_block
end
