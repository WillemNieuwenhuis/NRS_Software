pro nrs_stack_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_stack_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_stack_contentPanel = widget_base(uname = 'nrs_stack_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Stack layers')

  nrs_stack_mainPanel = widget_base(nrs_stack_contentPanel, /frame, /col)

  nrs_stack_use_folder_base = widget_base(nrs_stack_mainPanel, /nonexclusive, uname = 'nrs_stack_use_folder_base')
  nrs_stack_use_folder_button = widget_button(nrs_stack_use_folder_base $
                , uname = 'nrs_stack_use_folder_button' $
                , value = 'Use list file' $
                , event_pro = 'nrs_stack_use_folder_toggle' $
              )

  nrs_stack_folder = cw_dirfile(nrs_stack_mainPanel $
                , title = 'Input folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_folder' $
              )

  nrs_stack_filelist = cw_dirfile(nrs_stack_mainPanel $
                , title = 'Input list file' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_filelist' $
              )

  nrs_stack_output_panel = widget_base(nrs_stack_contentPanel, /frame, /col)
  nrs_stack_outputFile = cw_dirfile(nrs_stack_output_panel, uname = 'nrs_stack_outputFile' $
        , style = 'file' $
        , title = 'Output basename' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_stack_contentPanel $
                , ok_uname = 'nrs_stack_gobutton', ok_value = 'Go!', ok_tooltip = 'Combine separate layers into a single stack' $
                , cancel_uname = 'nrs_stack_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_contentpanel

  ; Initialize stuff
  widget_control, nrs_stack_folder, sensitive = 1
  widget_control, nrs_stack_filelist, sensitive = 0

  XManager, 'nrs_stack_gui', nrs_stack_contentPanel, /no_block
end
