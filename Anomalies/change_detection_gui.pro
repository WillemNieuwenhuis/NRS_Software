; Add a menu item to the NRS menu
pro change_detection_gui_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Change detection...', $
    UVALUE = 'Detect changes', EVENT_PRO = 'change_detection_menu', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

pro change_detection_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='change_detection_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='change_detection_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        change_detection_handleGo, Event
    end
  else:

  endcase

end
;
; Empty stub procedure used for autoloading.
;
pro change_detection_gui, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  change_detection_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
end

pro change_detection_menu, event
  change_detection_mainPanel
end

pro change_detection_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0), $
    input_fid:  long(0) $
  }

  label_width = 130
  label_wide_width = 150
  text_width =  70
  text_small_width = 5
  num_width =   15

  change_detection_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'change_detection_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Change detection')

  change_detection_inputPanel = widget_base(change_detection_contentPanel, /frame, /col)
  change_detection_inputImage = cw_dirfile(change_detection_inputPanel $
                , title = 'Input NDVI image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_inputImage' $
                , event_pro = 'change_detection_handleBrowseInput' $
              )
              
  change_detection_ndvipy = fsc_inputfield(change_detection_inputPanel $
                , uname = 'change_detection_ndvipy' $
                , title = 'NDVI layers per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '36' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_handle_NPY_change' $
              )

  change_detection_year_panel = widget_base(change_detection_inputPanel $
                , /row $
                , sensitiv = 0 $
                , uname = 'change_detection_year_panel' $
              )
  change_detection_years_label = widget_label(change_detection_year_panel $
                , xsize = label_width $
                , value = 'Select year' $
              )
  change_detection_years_combo = widget_combobox(change_detection_year_panel $
                , uname = 'change_detection_years_combo' $
                , value = ['All'] $
              )

  change_detection_time_panel = widget_base(change_detection_inputPanel $
                , /row $
                , sensitiv = 0 $
                , uname = 'change_detection_time_panel' $
              )
              
  change_detection_time_from = fsc_inputfield(change_detection_time_panel $
                , uname = 'change_detection_time_from' $
                , title = 'Choose period: From' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1' $
                , xsize = text_small_width $
                , uvalue = 1 $
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_handle_time_ft' $
              )

  change_detection_time_to = fsc_inputfield(change_detection_time_panel $
                , uname = 'change_detection_time_to' $
                , title = 'To' $
                , labelalign = 0 $
                , labelsize = 0 $
                , value = '36' $
                , xsize = text_small_width $
                , uvalue = 36 $   ; use uvalue as memory for the value
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_handle_time_ft' $
              )

  change_detection_segments = cw_dirfile(change_detection_inputPanel $
                , title = 'Input segments image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_segments' $
              )
              
  change_detection_classes = cw_dirfile(change_detection_inputPanel $
                , title = 'Input class image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_classes' $
              )
              
  change_detection_sdTable = cw_dirfile(change_detection_inputPanel $
                , title = 'Input stdev table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables', 'All files']] $
                , uname = 'change_detection_sdTable' $
              )

  change_detection_mask = cw_dirfile(change_detection_inputPanel $ 
                , title = 'Mask' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_mask' $
     )

  change_detection_outputPanel = widget_base(change_detection_contentPanel, /frame, /col)
  change_detection_outputImage = cw_dirfile(change_detection_outputPanel $
                , title = 'Output image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_outputImage' $
                , event_pro = 'change_detection_handle_outputfile' $
              )

  change_detection_sequencePanel = widget_base(change_detection_outputPanel, /col, title = 'Sequence')

  change_detection_countPanel = widget_base(change_detection_sequencePanel, title='Sequence aggregation', /col, /nonexclusive)
  change_detection_counting = widget_button(change_detection_countPanel, uname='change_detection_counting'  $
                , /align_left $
                , value='Find minimal number of anomalies (per year)' $
                , event_pro = 'change_detection_toggle_counting' $
              )
  change_detection_countingDetailPanel = widget_base(change_detection_sequencePanel, /col, uname='change_detection_countingDetailPanel', sensitive = 0)
  cd_count_line_panel = widget_base(change_detection_countingDetailPanel, /row)
  change_detection_countingCount = fsc_inputfield(cd_count_line_panel $
                , uname = 'change_detection_countingCount' $
                , title = 'Anomaly count' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '66' $
                , xsize = text_small_width $
                , UnitText = '%' $
                , /integervalue $
                , /all_events $
                , event_func = 'change_detection_handle_count' $
              )
  cd_convert_label = widget_label(cd_count_line_panel, value = '  ', /dynamic_resize, uname = 'cd_convert_label')
  
  change_detection_outputCounting = cw_dirfile(change_detection_countingDetailPanel $
                , title = 'Counting image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_outputCounting' $
              )

  change_detection_mag4count_panel = widget_base(change_detection_countingDetailPanel, title='Magnitude aggregation', /col, /nonexclusive)
  change_detection_mag4count = widget_button(change_detection_mag4count_panel, uname='change_detection_mag4count'  $
                , /align_left $
                , value='Generate probability of change (per year)' $
                , event_pro = 'change_detection_toggle_magcounting' $
              )

  change_detection_mag_panel = widget_base(change_detection_countingDetailPanel, /col, uname='change_detection_mag_panel', sensitive = 0)
  change_detection_magnitude = cw_dirfile(change_detection_mag_panel $
                , title = 'Probability of change image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'change_detection_magnitude' $
              )
              
  change_detection_mag_aggr_panel = widget_base(change_detection_mag_panel, /row)
  change_detection_mag_aggr_label = widget_label(change_detection_mag_aggr_panel $
                , value = 'Aggregation method' $
                , xsize = label_width $
              )
  change_detection_mag_aggr_combo = widget_combobox(change_detection_mag_aggr_panel $
                , uname = 'change_detection_mag_aggr_combo' $
                , value = ['sum', 'avg', 'min', 'max'] $
              )

  change_detection__createButtonPanel, change_detection_contentPanel

  ; Make sure we create the form
  Widget_Control, /REALIZE, change_detection_contentPanel

  ; Initialize stuff
  void = change_detection_handle_count({top:change_detection_contentPanel})
  state.parent = change_detection_contentPanel
  widget_control, change_detection_contentPanel, set_uvalue = state

  XManager, 'change_detection_gui', change_detection_contentPanel, /NO_BLOCK
 end

pro change_detection__createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  Buttonpanel = Widget_Base(parent $
    , /ALIGN_RIGHT $
    , /Row, SPACE = 3 $
    , XPAD = 3 $
    )

  change_detection_GoButton = Widget_Button(Buttonpanel, UNAME='change_detection_GoButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Find best spectral matches' ,VALUE='Go!')

  change_detection_CancelButton = Widget_Button(Buttonpanel, UNAME='change_detection_CancelButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Cancel the operation' ,VALUE='Close')

end
