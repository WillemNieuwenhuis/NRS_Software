pro nrs_stack_quartiles_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_quartiles_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_quartiles_exec, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_quartiles_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_stack_quartiles_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_stack_quartiles_contentPanel = widget_base(uname = 'nrs_stack_quartiles_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Stack quartiles')

  nrs_stack_quartiles_mainPanel = widget_base(nrs_stack_quartiles_contentPanel, /frame, /col)

  nrs_stack_quartiles_image = cw_dirfile(nrs_stack_quartiles_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_quartiles_image' $
                , event_pro = 'nrs_stack_quartiles_image_change' $
              )

  nrs_stack_quartiles_output_panel = widget_base(nrs_stack_quartiles_contentPanel, /frame, /col)
  nrs_stack_quartiles_outputFile = cw_dirfile(nrs_stack_quartiles_output_panel $
        , uname = 'nrs_stack_quartiles_outputFile' $
        , style = 'file' $
        , title = 'Output quartiles' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_stack_quartiles_exact_base = widget_base(nrs_stack_quartiles_output_panel, /nonexclusive, uname = 'nrs_stack_quartiles_use_folder_base')
  nrs_stack_quartiles_exact_button = widget_button(nrs_stack_quartiles_exact_base $
                , uname = 'nrs_stack_quartiles_exact_button' $
                , value = 'Exact quartiles' $
              )

  nrs_gui_createButtonPanel, nrs_stack_quartiles_contentPanel $
                , ok_uname = 'nrs_stack_quartiles_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate temporal quartiles' $
                , cancel_uname = 'nrs_stack_quartiles_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_quartiles_contentpanel

  ; Initialize stuff
  widget_control, nrs_stack_quartiles_exact_button, set_button = 1

  XManager, 'nrs_stack_quartiles_gui', nrs_stack_quartiles_contentPanel, /no_block
end
