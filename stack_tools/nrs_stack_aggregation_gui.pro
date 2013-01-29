pro nrs_stack_aggregation_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_aggregation_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_aggregation_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_aggregation_handleGo, Event
    end
  else:

  endcase

end

pro nrs_stack_aggregation_gui, event
  state = { $
    parent:   long(0) $
  }
  
  list_items = { $
    nb : long(-1), $
    ns : long(0), $
    nl : long(0), $
    items : ptr_new() $
  }

  label_width = 130
  label_wide_width = 150
  text_width =  70
  text_small_width = 5
  num_width =   15

  nrs_stack_aggregation_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'nrs_stack_aggregation_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Stack: bandwise aggregation')


  nrs_stack_aggregation_inputstack = cw_dirfile(nrs_stack_aggregation_contentPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_aggregation_inputstack' $
                , event_pro = 'nrs_stack_aggregation_handle_inputfile' $
              )

  wl_add_rem_panel = widget_base(nrs_stack_aggregation_contentPanel, /row)
  wl_add_button = widget_button(wl_add_rem_panel $
                , uname = 'nrs_stack_aggregation_add' $
                , value = 'Add' $
                , event_pro = 'nrs_stack_aggregation_add' $
                , sensitive = 0 $
              )
  wl_rem_button = widget_button(wl_add_rem_panel $
                , uname = 'nrs_stack_aggregation_remove' $
                , value = 'Remove' $
                , event_pro = 'nrs_stack_aggregation_remove' $
                , sensitive = 0 $
              )
  
  wl_label = widget_label(nrs_stack_aggregation_contentPanel $
                , value = 'List of stacks' $
                , uname = 'nrs_stack_aggregation_label' $
                , xsize = label_wide_width $
                , /align_left $
              )
                
  wl = widget_list(nrs_stack_aggregation_contentPanel $
                , uname = 'nrs_stack_aggregation_list' $
                , xsize = text_width $
                , ysize = 5 $
                , event_pro = 'nrs_stack_aggregation_listev' $
              )
  
  nrs_stack_aggregation_panel = widget_base(nrs_stack_aggregation_contentPanel, /row)
  nrs_stack_aggregation_aggr_label = widget_label(nrs_stack_aggregation_panel $
                , value = 'Aggregation method' $
                , xsize = label_width $
              )
  nrs_stack_aggregation_aggr_combo = widget_combobox(nrs_stack_aggregation_panel $
                , uname = 'nrs_stack_aggregation_aggr_combo' $
                , value = ['Sum', 'Mean', 'Median', 'Min', 'Max'] $
              )

  nrs_stack_aggregation__createButtonPanel, nrs_stack_aggregation_contentPanel

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_aggregation_contentpanel


  ; Initialize stuff
  widget_control, wl, set_uvalue = list_items
  
  state.parent = nrs_stack_aggregation_contentPanel
  widget_control, nrs_stack_aggregation_contentPanel, set_uvalue = state

  XManager, 'nrs_stack_aggregation_gui', nrs_stack_aggregation_contentPanel, /NO_BLOCK
end

pro nrs_stack_aggregation__createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  buttonpanel = widget_base(parent $
    , /align_right $
    , /row, space = 3 $
    , xpad = 3 $
    )

  nrs_stack_aggregation_GoButton = widget_button(buttonpanel, uname='nrs_stack_aggregation_GoButton'  $
    ,scr_xsize=50 ,scr_ysize=22  $
    ,/align_center $
    ,tooltip='Find best spectral matches' ,value='Go!')

  nrs_stack_aggregation_CancelButton = widget_button(buttonpanel, uname='nrs_stack_aggregation_CancelButton'  $
    ,scr_xsize=50 ,scr_ysize=22  $
    ,/align_center $
    ,tooltip='Cancel the operation' ,value='Close')

end
