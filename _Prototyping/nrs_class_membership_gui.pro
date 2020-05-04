pro nrs_class_membership_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_class_membership_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_class_membership_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_class_membership_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_class_membership_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_class_membership_contentPanel = widget_base(uname = 'nrs_class_membership_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Determine class membership')

  nrs_class_membership_mainPanel = widget_base(nrs_class_membership_contentPanel, /frame, /col)

  nrs_class_membership_image = cw_dirfile(nrs_class_membership_mainPanel $
    , title = 'Input image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_class_membership_image' $
    , event_pro = 'nrs_class_membership_handle_input' $
    )

  nrs_class_membership_kernel = fsc_inputfield(nrs_class_membership_mainPanel $
    , uname = 'nrs_class_membership_kernel' $
    , title = 'Kernel size' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '3' $
    , xsize = num_width $
    , /all_events $
    )

  nrs_class_membership_ignore = fsc_inputfield(nrs_class_membership_mainPanel $
    , uname = 'nrs_class_membership_ignore' $
    , title = 'No data value' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '' $
    , xsize = num_width $
    , /all_events $
    )

  nrs_class_membership_output_panel = widget_base(nrs_class_membership_contentPanel, /frame, /col)
  nrs_class_membership_outputFolder = cw_dirfile(nrs_class_membership_output_panel $
    , uname = 'nrs_class_membership_output' $
    , style = 'file' $
    , title = 'Output base name' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_class_membership_contentPanel $
    , ok_uname = 'nrs_class_membership_gobutton', ok_value = 'Run', ok_tooltip = 'Determine class membership' $
    , cancel_uname = 'nrs_class_membership_cancelbutton', cancel_value = 'Close', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_class_membership_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_class_membership_gui', nrs_class_membership_contentPanel, /no_block
end
