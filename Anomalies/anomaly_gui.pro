; Add a menu item to the NRS menu
pro anomaly_gui_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Detect NDVI Anomalies', $
    UVALUE = 'Detect NDVI Anomalies', EVENT_PRO = 'anomaly_menu', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

pro anomaly_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='anomaly_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='anomaly_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        anomaly_handleGo, Event
    end
  else:

  endcase

end
;
; Empty stub procedure used for autoloading.
;
pro anomaly_gui, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  anomaly_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
end

pro anomaly_menu, event
  anomaly_mainPanel
end

pro anomaly_mainPanel, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0), $
    input_fid:  long(0) $
  }

  label_width = 130
  label_wide_width = 150
  text_width =  70
  num_width =   15

  anomaly_contentPanel = Widget_Base(GROUP_LEADER = wGroup, UNAME = 'anomaly_contentPanel'  $
    , /col  $
    , TITLE = 'Anomaly detection')

  anomaly_inputPanel = widget_base(anomaly_contentPanel, /frame, /col)
  anomaly_inputImage = cw_dirfile(anomaly_inputPanel $
                , title = 'Input NDVI image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_inputImage' $
                , event_pro = 'anomaly_handleBrowseInput' $
              )
              
  anomaly_classes = cw_dirfile(anomaly_inputPanel $
                , title = 'Input class image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_classes' $
              )
              
  anomaly_inputTable = cw_dirfile(anomaly_inputPanel $
                , title = 'Input stdev range table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables', 'All files']] $
                , uname = 'anomaly_inputTable' $
              )
              
  anomaly_sdTable = cw_dirfile(anomaly_inputPanel $
                , title = 'Input stdev table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables', 'All files']] $
                , uname = 'anomaly_sdTable' $
                , sensitive = 0 $
              )
              
  anomaly_outputPanel = widget_base(anomaly_contentPanel, /frame, /col)
  anomaly_outputImage = cw_dirfile(anomaly_outputPanel $
                , title = 'Output image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_outputImage' $
                , event_pro = 'anomaly_handle_outputfile' $
              )

  anomaly_scenarioTabs = widget_tab(anomaly_contentPanel, uname = 'anomaly_scenarioTabs' $
                , event_pro = 'anomaly_handle_tabchange' $ 
              )
  
  anomaly_sequencePanel = widget_base(anomaly_scenarioTabs, /col, title = 'Sequence')
  anomaly_aggregatePanel = widget_base(anomaly_sequencePanel, title='Sequence aggregation', /col, /nonexclusive)
  anomaly_aggregate = widget_button(anomaly_aggregatePanel, uname='anomaly_aggregate'  $
                , /align_left $
                , value='Find largest consecutive anomaly (per year)' $
                , event_pro = 'anomaly_toggle_aggregate' $
              )
  anomaly_sequenceDetailPanel = widget_base(anomaly_sequencePanel, /col, uname='anomaly_sequencePanel', sensitive = 0)
  anomaly_sequenceCount = cw_field_ex(anomaly_sequenceDetailPanel $
                , uname = 'anomaly_sequenceCount' $
                , title = 'Sequence length' $
                , tsize = label_width $
                , value = '18' $
                , /integer $
              )

  anomaly_outputSequence = cw_dirfile(anomaly_sequenceDetailPanel $
                , title = 'Sequence image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_outputSequence' $
              )

  anomaly_countPanel = widget_base(anomaly_sequencePanel, title='Sequence aggregation', /col, /nonexclusive)
  anomaly_counting = widget_button(anomaly_countPanel, uname='anomaly_counting'  $
                , /align_left $
                , value='Find minimal number of anomalies (per year)' $
                , event_pro = 'anomaly_toggle_counting' $
              )
  anomaly_countingDetailPanel = widget_base(anomaly_sequencePanel, /col, uname='anomaly_countingDetailPanel', sensitive = 0)
  anomaly_countingCount = cw_field_ex(anomaly_countingDetailPanel $
                , uname = 'anomaly_countingCount' $
                , title = 'Anomaly count' $
                , tsize = label_width $
                , value = '18' $
                , /integer $
              )

  anomaly_outputCounting = cw_dirfile(anomaly_countingDetailPanel $
                , title = 'Counting image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_outputCounting' $
              )

  anomaly_mag4count_panel = widget_base(anomaly_countingDetailPanel, title='Magnitude aggregation', /col, /nonexclusive)
  anomaly_mag4count = widget_button(anomaly_mag4count_panel, uname='anomaly_mag4count'  $
                , /align_left $
                , value='Generate probability of change (per year)' $
                , event_pro = 'anomaly_toggle_magcounting' $
              )

  anomaly_mag_panel = widget_base(anomaly_countingDetailPanel, /col, uname='anomaly_mag_panel', sensitive = 0)
  anomaly_magnitude = cw_dirfile(anomaly_mag_panel $
                , title = 'Probability of change image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_magnitude' $
              )

  anomaly_buildSeasonPanel, anomaly_scenarioTabs, 'Seasonal', label_width, text_width
  
  anomaly_buildChangePanel, anomaly_scenarioTabs, 'Change detection', label_width, text_width
  
  anomaly__createButtonPanel, anomaly_contentPanel

  ; Make sure we create the form
  Widget_Control, /REALIZE, anomaly_contentPanel

  ; Initialize stuff
  state.parent = anomaly_contentPanel
  widget_control, anomaly_contentPanel, set_uvalue = state

  XManager, 'anomaly_gui', anomaly_contentPanel, /NO_BLOCK
 end

pro anomaly__createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  Buttonpanel = Widget_Base(parent $
    , /ALIGN_RIGHT $
    , /Row, SPACE = 3 $
    , XPAD = 3 $
    )

  anomaly_GoButton = Widget_Button(Buttonpanel, UNAME='anomaly_GoButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Find best spectral matches' ,VALUE='Go!')

  anomaly_CancelButton = Widget_Button(Buttonpanel, UNAME='anomaly_CancelButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Cancel the operation' ,VALUE='Close')

end

pro anomaly_buildYearlyPanel, parent
end

pro anomaly_buildChangePanel, parent, pageTitle, label_width, text_width
  changePanel = widget_base(parent, /col, title = pageTitle)
  anomaly_mag_output = cw_dirfile(changePanel $
      , title = 'Magnitude output' $
      , style = 'file' $
      , xsize = text_width $
      , xtitlesize = label_width $
      , uname = 'anomaly_mag_output' $
    )

  anomaly_refimg2 = cw_dirfile(changePanel $ 
                , title = 'Coarse class image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_refimg2' $
     )
  anomaly_mask = cw_dirfile(changePanel $ 
                , title = 'Mask' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_mask' $
     )

  crlf = string(13b) + string(10b)
  anomaly_ChangeInfoText = widget_text(changePanel, /wrap $
      , uname = 'anomaly_ChangeInfoText' $
      , ysize = 7 $
      , xsize = text_width $
      , value = 'Change detection by' + crlf $
        + '(1)  selection locations by class, then' + crlf $
        + '(2)  averaging these,' + crlf $
        + '(3)  subtract the average from the selection, then optionally' + crlf $
        + '(3a) selected location by second (courser grained) classes, then finally' + crlf $ 
        + '(4)  compare with a standard deviation lookup table: a change is detected when' + crlf $
        + '     the difference value is greater than the lookup table value.' $ 
    )
end

pro anomaly_buildSeasonPanel, parent, pageTitle, label_width, text_width
  seasonPanel = widget_base(parent, /col, title = pageTitle)

  seasons = ['Year 1, first','Year 1, second' $
            ,'Year 2, first','Year 2, second' $
            ,'Year 3, first','Year 3, second' $
            ,'Year 4, first','Year 4, second' $
            ,'Year 5, first','Year 5, second']
  for i = 1, 5 do $
    anomaly_buildYOS, seasonPanel, i * 2 - 2, i * 2 - 1, seasons[i * 2 - 2], seasons[i * 2 - 1] 

  anomaly_outputImage = cw_dirfile(seasonPanel $
                , title = 'Seasonal image' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'anomaly_outputSeasons' $
              )
end

pro anomaly_buildYOS, parent, first, second, fname, sname
  p = widget_base(parent, /col, /frame)
  anomaly_buildOneSeason, p, fname, 100, first, [1,18], 0.75
  anomaly_buildOneSeason, p, sname, 100, second, [19, 36], 0.75
end

pro anomaly_buildOneSeason, parent, seasonName, titleSize, num, default_period, default_prop
  osp = widget_base(parent, /row)
  lbl = widget_label(osp, value = seasonName, xsize = titleSize)
  numstr = string(num, format = '(i03)')
  seas1 = widget_text(osp, xsize = 10, /editable, uname = 'seasfirst_' + numstr, value = string(default_period[0], format = '(i0)'))
  seas2 = widget_text(osp, xsize = 10, /editable, uname = 'seaslast_' + numstr, value = string(default_period[1], format = '(i0)'))
  proplbl = widget_label(osp, value = '    Proportion (%) ')
  prop = widget_text(osp, xsize = 10, /editable, uname = 'prop_' + numstr, value = string(default_prop * 100, format = '(f0.1)'))
end
