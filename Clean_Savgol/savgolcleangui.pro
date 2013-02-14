pro nrs_SavgolCleanGUI_define_buttons, buttonInfo
  envi_define_menu_button, buttoninfo, value = 'Savitzki-Golay filter', $
    uvalue = 'Savitzki-Golay filter', /menu, $ ;/sibling, $
    ref_value = 'NRS', position = 'last'
end

pro nrs_SavgolCleanGUI_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_SavgolClean_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_SavgolClean_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_SavgolClean_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_SavgolCleanGUI, group_leader = wgroup
  label_width = 75
  label_wide_width = 150
  text_width =  70
  num_width =   15
  param_off_x = 135
  param_off_y = 5

  top = widget_base(group_leader = wgroup, uname = 'svc_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , title = 'Data cleaning with Savitzky-Golay')
  
  svc_basePanel = widget_base(top, /col)
  svc_inputPanel = widget_base(svc_basePanel, /col, /frame)
  
  svc_input_file = cw_dirfile(svc_inputPanel, title = 'Input CSV file' $
      , xsize = text_width, xtitlesize = label_width $
      , event_pro = 'svc_handleInputFileChange' $
      , style='file' $
      , uname='svc_input_file' $
    )

  svc_fld = widget_label(svc_inputPanel, value = 'Select one or more fields to filter:', /align_left)
  svc_field_panel = widget_base(svc_inputPanel, /row)
  svc_field_label = widget_label(svc_field_panel, value = '' $
      , xsize = label_width $
    )
  svc_field_list = widget_list(svc_field_panel $
      , uname = 'svc_field_list' $
      , /multiple $
      , xsize = 20 $
      , ysize = 7 $
    )
  
  svc_filter_panel = widget_base(svc_basePanel, /col, /frame)
  svc_fld = widget_label(svc_filter_panel, value = 'Savitzky-Golay filter parameters', /align_left)
  svc_width_field = cw_field_ex(svc_filter_panel $
      , value = 33 $
      , title = 'Width', tsize = label_width $
      , /integer, uname = 'svc_width_field' $
    )
  svc_degree_field = cw_field_ex(svc_filter_panel $
      , value = 4 $
      , title = 'Degree', tsize = label_width $
      , /integer, uname = 'svc_degree_field' $
    )
  svc_order_field = cw_field_ex(svc_filter_panel $
      , value = 0 $
      , title = 'Order', tsize = label_width $
      , /integer, uname = 'svc_order_field' $
    )

  svc_output_file = cw_dirfile(svc_basePanel, title = 'Output CSV file' $
      , /frame $
      , xsize = text_width, xtitlesize = label_width $
      , style='file' $
      , uname='svc_output_file' $
    )
    
  nrs_gui_createButtonPanel, top $
                , ok_uname = 'nrs_SavgolClean_gobutton', ok_value = 'Go!', ok_tooltip = 'Clean data with Savitzki-Golay filter' $
                , cancel_uname = 'nrs_SavgolClean_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'


  ; Make sure we create the form
  Widget_Control, /realize, top

  ; Initialize stuff
;  state.parent = top
;  widget_control, top, set_uvalue = state
  
  ; show window
  XManager, 'nrs_SavgolCleanGUI', top, /no_block
  
end