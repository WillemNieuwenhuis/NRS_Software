; Add a menu item to the NRS menu
pro change_detection_v2_gui_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Probability of change...', $
    UVALUE = 'Detect changes', EVENT_PRO = 'change_detection_v2_menu', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

pro change_detection_v2_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='change_detection_v2_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='change_detection_v2_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        change_detection_v2_handleGo, Event
    end
  else:

  endcase

end
;
; Empty stub procedure used for autoloading.
;
pro change_detection_v2_gui, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  change_detection_v2_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
end

pro change_detection_v2_menu, event
  change_detection_v2_mainPanel
end

pro change_detection_v2_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0) $
  }

  label_width = 180
  label_wide_width = 150
  text_width =  70
  text_small_width = 5
  num_width =   15

  change_detection_v2_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'change_detection_v2_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Change detection')

  change_detection_v2_inputPanel = widget_base(change_detection_v2_contentPanel, /frame, /col)
  
  change_detection_v2_refimage = cw_dirfile(change_detection_v2_inputPanel $
                , title = 'NDVI data stack (reference period)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_v2_refimage' $
              )

  change_detection_v2_classes = cw_dirfile(change_detection_v2_inputPanel $
                , title = 'Classified NDVI map (raster)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_v2_classes' $
              )

  change_detection_v2_inputImage = cw_dirfile(change_detection_v2_inputPanel $
                , title = 'NDVI data stack (for change analysis)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_v2_inputImage' $
                , event_pro = 'change_detection_v2_handleBrowseInput' $
              )

  group = cw_groupbox(change_detection_v2_inputPanel, group_title = 'Parameters')              
  change_detection_v2_ndvipy = fsc_inputfield(group $
                , uname = 'change_detection_v2_ndvipy' $
                , title = 'NDVI layers per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '36' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_v2_handle_NPY_change' $
              )
              
  change_detection_v2_year_panel = widget_base(group $
                , /row $
                , sensitiv = 0 $
                , uname = 'change_detection_v2_year_panel' $
              )
  change_detection_v2_years_label = widget_label(change_detection_v2_year_panel $
                , xsize = label_width $
                , value = 'Select year(s)' $
              )
  change_detection_v2_years_combo = widget_combobox(change_detection_v2_year_panel $
                , uname = 'change_detection_v2_years_combo' $
                , value = ['All'] $
              )

  change_detection_v2_time_panel = widget_base(group $
                , /row $
                , sensitiv = 0 $
                , uname = 'change_detection_v2_time_panel' $
              )
              
  change_detection_v2_time_from = fsc_inputfield(change_detection_v2_time_panel $
                , uname = 'change_detection_v2_time_from' $
                , title = 'Select period in the year; From' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1' $
                , xsize = text_small_width $
                , uvalue = 1 $
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_v2_handle_time_ft' $
              )

  change_detection_v2_time_to = fsc_inputfield(change_detection_v2_time_panel $
                , uname = 'change_detection_v2_time_to' $
                , title = 'To' $
                , labelalign = 0 $
                , labelsize = 0 $
                , value = '36' $
                , xsize = text_small_width $
                , uvalue = 36 $   ; use uvalue as memory for the value
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_v2_handle_time_ft' $
              )

  change_detection_v2_sd_panel = widget_base(group $
                , /row $
                , uname = 'change_detection_v2_sd_panel' $
              )

  change_detection_v2_sd_label = widget_label(change_detection_v2_sd_panel $
                , xsize = label_width $
                , value = 'Threshold' $
              )

  change_detection_sd_muly_combo = widget_combobox(change_detection_v2_sd_panel $
                , uname = 'change_detection_sd_muly_combo' $
                , value = ['1', '1.5', '2', '2.5', '3', '3.5'] $
              )

  change_detection_v2_sd_unit_label = widget_label(change_detection_v2_sd_panel $
;                , xsize = label_width $
                , value = ' * SD' $
              )

  cd_mask_base = widget_base(group, /row)
  change_detection_v2_mask = fsc_inputfield(cd_mask_base $
                , uname = 'change_detection_v2_mask' $
                , title = 'Excluded areas (less than)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '2' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
              )
              
  void = widget_label(cd_mask_base, value = 'pixels')
              
;  change_detection_v2_outputPanel = widget_base(change_detection_v2_contentPanel, /frame, /col)

  change_detection_v2_outputPanel = cw_groupbox(change_detection_v2_contentPanel, group_title = 'Output')              
  change_detection_v2_magnitude = cw_dirfile(change_detection_v2_outputPanel $
                , title = 'Probability of change map(s)' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_v2_magnitude' $
              )
              
  change_detection__v2_createButtonPanel, change_detection_v2_contentPanel

  ; Make sure we create the form
  Widget_Control, /REALIZE, change_detection_v2_contentPanel

  ; Initialize stuff
  state.parent = change_detection_v2_contentPanel
  widget_control, change_detection_v2_contentPanel, set_uvalue = state

  XManager, 'change_detection_v2_gui', change_detection_v2_contentPanel, /NO_BLOCK
 end

pro change_detection__v2_createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  Buttonpanel = Widget_Base(parent $
    , /ALIGN_RIGHT $
    , /Row, SPACE = 3 $
    , XPAD = 3 $
    )

  change_detection_v2_GoButton = Widget_Button(Buttonpanel, UNAME='change_detection_v2_GoButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Find best spectral matches' ,VALUE='Go!')

  change_detection_v2_CancelButton = Widget_Button(Buttonpanel, UNAME='change_detection_v2_CancelButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Cancel the operation' ,VALUE='Close')

end
