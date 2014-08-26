pro nrs_interpolate_to_payment_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  
  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_interpolate_to_payment_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_interpolate_to_payment_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_interpolate_to_payment_handlego, Event
    end
    else:
    
  endcase
  
end

pro nrs_interpolate_to_payment_gui, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0) $
  }
  
  label_width = 180
  label_wide_width = 150
  text_width =  70
  text_small_width = 10
  num_width =   15
  
  nrs_interpolate_to_payment_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'nrs_interpolate_to_payment_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Determine pay percentage')
    
  nrs_interpolate_to_payment_mainPanel = widget_base(nrs_interpolate_to_payment_contentPanel, /frame, /col)
  
  nrs_interpolate_to_payment_inputPanel = cw_groupbox(nrs_interpolate_to_payment_mainPanel, group_title = 'Timeseries')
  
  nrs_interpolate_to_payment_refimage = cw_dirfile(nrs_interpolate_to_payment_inputPanel $
    , title = 'NDVI timeseries' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_interpolate_to_payment_refimage' $
    , event_pro = 'nrs_interpolate_to_payment_handle_input' $
    )
    
  nrs_interpolate_to_payment_classes = cw_dirfile(nrs_interpolate_to_payment_inputPanel $
    , title = 'Classified NDVI image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_interpolate_to_payment_classes' $
    )
    
  nrs_interpolate_to_payment_season_p5table = cw_dirfile(nrs_interpolate_to_payment_inputPanel $
    , title = '5% percentile table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_interpolate_to_payment_season_p5table' $
    )
    
  nrs_interpolate_to_payment_season_p25table = cw_dirfile(nrs_interpolate_to_payment_inputPanel $
    , title = '25% percentile table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_interpolate_to_payment_season_p25table' $
    )
    
  nrs_interpolate_to_payment_start_date = fsc_inputfield(nrs_interpolate_to_payment_inputPanel $
    , uname = 'nrs_interpolate_to_payment_start_date' $
    , title = 'Start date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )
    
  nrs_interpolate_to_payment_end_date = fsc_inputfield(nrs_interpolate_to_payment_inputPanel $
    , uname = 'nrs_interpolate_to_payment_end_date' $
    , title = 'End date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )
    
  group = cw_groupbox(nrs_interpolate_to_payment_mainPanel, group_title = 'Parameters')
  nrs_interpolate_to_payment_imgperyear = fsc_inputfield(group $
    , uname = 'nrs_interpolate_to_payment_imgperyear' $
    , title = 'NDVI layers per year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '36' $
    , xsize = text_small_width $
    , /integervalue $
    , /all_events $
    )
    
  nrs_interpolate_to_payment_outputPanel = widget_base(nrs_interpolate_to_payment_contentPanel, /frame, /col)
  nrs_interpolate_to_payment_output = cw_dirfile(nrs_interpolate_to_payment_outputPanel $
    , title = 'Output timeseries' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_interpolate_to_payment_output' $
    )
    
  nrs_gui_createbuttonpanel, nrs_interpolate_to_payment_contentPanel $
    , ok_uname = 'nrs_interpolate_to_payment_gobutton', ok_value = 'Go!', ok_tooltip = 'Determine pay percentage' $
    , cancel_uname = 'nrs_interpolate_to_payment_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'
    
    
  ; Make sure we create the form
  widget_control, /REALIZE, nrs_interpolate_to_payment_contentPanel
  
  ; Initialize stuff
  ;  widget_control, nrs_interpolate_to_payment_absdiff, /set_button
  state.parent = nrs_interpolate_to_payment_contentPanel
  widget_control, nrs_interpolate_to_payment_contentPanel, set_uvalue = state
  
  xmanager, 'nrs_interpolate_to_payment_gui', nrs_interpolate_to_payment_contentPanel, /NO_BLOCK
end

