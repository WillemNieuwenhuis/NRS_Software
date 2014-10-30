pro covercam_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='covercam_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='covercam_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        covercam_handleGo, Event
    end
  else:

  endcase

end

pro covercam_gui, event, GROUP_LEADER = wGroup, _EXTRA = _VWBExtra_
  state = { $
    parent:   long(0) $
  }

  label_width = 180
  label_wide_width = 150
  text_width =  70
  text_small_width = 5
  num_width =   15

  covercam_contentPanel = widget_base(GROUP_LEADER = wGroup, UNAME = 'covercam_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'CoverCam')

  covercam_inputPanel = widget_base(covercam_contentPanel, /frame, /col)
  
  covercam_refimage = cw_dirfile(covercam_inputPanel $
                , title = 'NDVI data stack (reference period)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'covercam_refimage' $
              )

  covercam_classes = cw_dirfile(covercam_inputPanel $
                , title = 'Classified NDVI map (raster)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'covercam_classes' $
              )

  covercam_inputImage = cw_dirfile(covercam_inputPanel $
                , title = 'NDVI data stack (for change analysis)' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'covercam_inputImage' $
                , event_pro = 'covercam_handleBrowseInput' $
              )

  group = cw_groupbox(covercam_inputPanel, group_title = 'Parameters')              
  covercam_ndvipy = fsc_inputfield(group $
                , uname = 'covercam_ndvipy' $
                , title = 'NDVI layers per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '36' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
                , event_pro = 'covercam_handle_NPY_change' $
              )
              
  covercam_year_panel = widget_base(group $
                , /row $
                , sensitiv = 0 $
                , uname = 'covercam_year_panel' $
              )
  covercam_years_label = widget_label(covercam_year_panel $
                , xsize = label_width $
                , value = 'Select year(s)' $
              )
  covercam_years_combo = widget_combobox(covercam_year_panel $
                , uname = 'covercam_years_combo' $
                , value = ['All'] $
              )

  covercam_time_panel = widget_base(group $
                , /row $
                , sensitiv = 0 $
                , uname = 'covercam_time_panel' $
              )
              
  covercam_time_from = fsc_inputfield(covercam_time_panel $
                , uname = 'covercam_time_from' $
                , title = 'Select period in the year; From' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1' $
                , xsize = text_small_width $
                , uvalue = 0 $  ; zero inidcates no user change yet
                , /integervalue $
                , /all_events $
;                , event_func = 'covercam_handle_time_ft' $
              )

  covercam_time_to = fsc_inputfield(covercam_time_panel $
                , uname = 'covercam_time_to' $
                , title = 'To' $
                , labelalign = 0 $
                , labelsize = 0 $
                , value = '36' $
                , xsize = text_small_width $
                , uvalue = 0 $   ; use uvalue as memory for the value; 0 indicates no user change yet
                , /integervalue $
                , /all_events $
                , event_func = 'covercam_handle_time_ft' $
              )

  covercam_sd_panel = widget_base(group $
                , /row $
                , uname = 'covercam_sd_panel' $
              )

  covercam_sd_label = widget_label(covercam_sd_panel $
                , xsize = label_width $
                , value = 'Threshold' $
              )

  change_detection_sd_muly_combo = widget_combobox(covercam_sd_panel $
                , uname = 'change_detection_sd_muly_combo' $
                , value = ['1', '1.5', '2', '2.5', '3', '3.5'] $
              )

  covercam_sd_unit_label = widget_label(covercam_sd_panel $
;                , xsize = label_width $
                , value = ' * SD' $
              )

  cd_mask_base = widget_base(group, /row)
  covercam_mask = fsc_inputfield(cd_mask_base $
                , uname = 'covercam_mask' $
                , title = 'Excluded areas (less than)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '2' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
              )
              
  void = widget_label(cd_mask_base, value = 'pixels')

  covercam_absdiff_panel = widget_base(group $
                , title = 'Absolute differences', /col, /nonexclusive)
  covercam_asperc = widget_button(covercam_absdiff_panel $
                , uname = 'covercam_asperc'  $
                , /align_left $
                , value = 'Magnitudes as percentages' $
                )

  covercam_absdiff = widget_button(covercam_absdiff_panel $
                , uname = 'covercam_absdiff'  $
                , /align_left $
                , value = 'Absolute differences only' $
              )
              
  covercam_outputPanel = cw_groupbox(covercam_inputPanel, group_title = 'Output')              
  covercam_magnitude = cw_dirfile(covercam_outputPanel $
                , title = 'Probability of change map(s)' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'covercam_magnitude' $
              )
              
  nrs_gui_createButtonPanel, covercam_contentPanel $ 
    , ok_uname = 'covercam_GoButton', ok_value = 'Go!', ok_tooltip = 'Find best spectral matches' $
    , cancel_uname = 'covercam_CancelButton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'
    

  ; Make sure we create the form
  Widget_Control, /REALIZE, covercam_contentPanel

  ; Initialize stuff
  widget_control, covercam_absdiff, /set_button
  state.parent = covercam_contentPanel
  widget_control, covercam_contentPanel, set_uvalue = state

  XManager, 'covercam_gui', covercam_contentPanel, /NO_BLOCK
 end
