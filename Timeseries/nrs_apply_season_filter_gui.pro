pro nrs_apply_season_filter_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  
  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_apply_season_filter_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_apply_season_filter_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_apply_season_filter_handlego, Event
    end
    else:
    
  endcase
  
end

pro nrs_apply_season_filter_gui, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0) $
  }
  
  label_width = 180
  label_wide_width = 150
  text_width =  70
  text_small_width = 10
  num_width =   15
  
  nrs_apply_season_filter_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'nrs_apply_season_filter_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Filter growing season')
    
  nrs_apply_season_filter_mainPanel = widget_base(nrs_apply_season_filter_contentPanel, /frame, /col)
  
  nrs_apply_season_filter_inputPanel = cw_groupbox(nrs_apply_season_filter_mainPanel, group_title = 'Timeseries')
  
  nrs_apply_season_filter_refimage = cw_dirfile(nrs_apply_season_filter_inputPanel $
    , title = 'NDVI timeseries' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_apply_season_filter_refimage' $
    , event_pro = 'nrs_apply_season_filter_handle_input' $
    )
    
  nrs_apply_season_filter_classes = cw_dirfile(nrs_apply_season_filter_inputPanel $
    , title = 'Classified NDVI image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_apply_season_filter_classes' $
    )
    
  nrs_apply_season_filter_season_table = cw_dirfile(nrs_apply_season_filter_inputPanel $
    , title = 'Growing season filter table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_apply_season_filter_season_table' $
    )
    
  nrs_apply_season_filter_start_date = fsc_inputfield(nrs_apply_season_filter_inputPanel $
    , uname = 'nrs_apply_season_filter_start_date' $
    , title = 'Start date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )
    
  nrs_apply_season_filter_end_date = fsc_inputfield(nrs_apply_season_filter_inputPanel $
    , uname = 'nrs_apply_season_filter_end_date' $
    , title = 'End date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width $
    , /all_events $
    )
    
  group = cw_groupbox(nrs_apply_season_filter_mainPanel, group_title = 'Parameters')
  nrs_apply_season_filter_imgperyear = fsc_inputfield(group $
    , uname = 'nrs_apply_season_filter_imgperyear' $
    , title = 'NDVI layers per year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '36' $
    , xsize = text_small_width $
    , /integervalue $
    , /all_events $
    )
    
  nrs_apply_season_filter_outputPanel = widget_base(nrs_apply_season_filter_contentPanel, /frame, /col)
  nrs_apply_season_filter_output = cw_dirfile(nrs_apply_season_filter_outputPanel $
    , title = 'Output timeseries' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_apply_season_filter_output' $
    )

  nrs_gui_createButtonPanel, nrs_apply_season_filter_contentPanel $
                , ok_uname = 'nrs_apply_season_filter_gobutton', ok_value = 'Go!', ok_tooltip = 'Filter growing season' $
                , cancel_uname = 'nrs_apply_season_filter_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'


  ; Make sure we create the form
  widget_control, /REALIZE, nrs_apply_season_filter_contentPanel
  
  ; Initialize stuff
;  widget_control, nrs_apply_season_filter_absdiff, /set_button
  state.parent = nrs_apply_season_filter_contentPanel
  widget_control, nrs_apply_season_filter_contentPanel, set_uvalue = state
  
  xmanager, 'nrs_apply_season_filter_gui', nrs_apply_season_filter_contentPanel, /NO_BLOCK
end

