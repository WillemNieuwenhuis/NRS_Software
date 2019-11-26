pro nrs_climate_weighted_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_climate_weighted_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climate_weighted_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_climate_weighted_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_climate_weighted_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_climate_weighted_contentPanel = widget_base(uname = 'nrs_climate_weighted_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Climatology (Daily)')

  nrs_climate_weighted_mainPanel = widget_base(nrs_climate_weighted_contentPanel, /frame, /col)

  nrs_climate_weighted_basefolder = cw_dirfile(nrs_climate_weighted_mainPanel $
    , title = 'Base folder' $
    , style = 'directory' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_climate_weighted_basefolder' $
;    , event_pro = 'nrs_climate_weighted_handle_input' $
    )

  nrs_climate_weighted_file_pattern = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_file_pattern' $
    , title = 'File pattern' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '2t*.tif' $
    , xsize = text_width $
    )

  nrs_climate_weighted_date_pattern = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_date_pattern' $
    , title = 'Date pattern' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '.yyyy.mm.dd' $
    , xsize = text_width $
    )

  nrs_climate_weighted_start_year = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_start_year' $
    , title = 'Start year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_climate_weighted_end_year = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_end_year' $
    , title = 'End year' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_climate_weighted_window = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_window' $
    , title = 'Window (N12)' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '30' $
    , xsize = text_medium_width $
    )

  nrs_climate_weighted_percentiles = fsc_inputfield(nrs_climate_weighted_mainPanel $
    , uname = 'nrs_climate_weighted_percentiles' $
    , title = 'Quantiles' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '[0, 0.25, 0.5, 0.75, 1.0]' $
    , xsize = text_width $
    )

  nrs_climate_weighted_subset_base = widget_base(nrs_climate_weighted_mainPanel, /nonexclusive, uname = 'nrs_climate_weighted_group_base')
  nrs_climate_weighted_subset_button = widget_button(nrs_climate_weighted_subset_base $
    , uname = 'nrs_climate_weighted_subset_button' $
    , value = 'Use spatial subset' $
    , event_pro = 'nrs_climate_weighted_subset_button_toggle' $
    )

  nrs_climate_weighted_bounds_outer = widget_base(nrs_climate_weighted_mainPanel, col = 2, uname = 'nrs_climate_weighted_bounds_outer')
  nrs_climate_weighted_bounds_left = widget_base(nrs_climate_weighted_bounds_outer, row = 2, xsize = label_width)
  nrs_climate_weighted_bounds_base = widget_base(nrs_climate_weighted_bounds_outer, row = 2, frame = 1)
  nrs_climate_weighted_bounds_xstart = fsc_inputfield(nrs_climate_weighted_bounds_base $
    , uname = 'nrs_climate_weighted_bounds_xstart' $
    , title = 'X start' $
    , labelalign = 1 $
    , labelsize = label_width / 2 $
    , xsize = text_medium_width $
    )
  nrs_climate_weighted_bounds_xnum = fsc_inputfield(nrs_climate_weighted_bounds_base $
    , uname = 'nrs_climate_weighted_bounds_xnum' $
    , title = 'X width' $
    , labelalign = 1 $
    , labelsize = label_width / 2 $
    , xsize = text_medium_width $
    )
  nrs_climate_weighted_bounds_ystart = fsc_inputfield(nrs_climate_weighted_bounds_base $
    , uname = 'nrs_climate_weighted_bounds_ystart' $
    , title = 'Y start' $
    , labelalign = 1 $
    , labelsize = label_width / 2 $
    , xsize = text_medium_width $
    )
  nrs_climate_weighted_bounds_ynum = fsc_inputfield(nrs_climate_weighted_bounds_base $
    , uname = 'nrs_climate_weighted_bounds_ynum' $
    , title = 'Y width' $
    , labelalign = 1 $
    , labelsize = label_width / 2 $
    , xsize = text_medium_width $
    )

  nrs_climate_weighted_output_panel = widget_base(nrs_climate_weighted_contentPanel, /frame, /col)
  nrs_climate_weighted_overwrite_base = widget_base(nrs_climate_weighted_output_panel, /nonexclusive, uname = 'nrs_climate_weighted_raster_base')
  nrs_climate_weighted_overwrite_button = widget_button(nrs_climate_weighted_overwrite_base $
    , uname = 'nrs_climate_weighted_overwrite_button' $
    , value = 'Overwrite existing output' $
    )

  nrs_climate_weighted_outputfolder = cw_dirfile(nrs_climate_weighted_output_panel $
    , title = 'Output folder' $
    , style = 'directory' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_climate_weighted_outputfolder' $
    )

  nrs_gui_createbuttonpanel, nrs_climate_weighted_contentPanel $
    , ok_uname = 'nrs_climate_weighted_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate zonal percentiles' $
    , cancel_uname = 'nrs_climate_weighted_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climate_weighted_contentpanel

  ; Initialize stuff
;  widget_control, nrs_climate_weighted_subset_button, sensitive = 1
  widget_control, nrs_climate_weighted_bounds_outer, sensitive = 0
  
;  nrs_climate_weighted_imgperyear->setproperty, sensitive = 0

  xmanager, 'nrs_climate_weighted_gui', nrs_climate_weighted_contentPanel, /no_block
end
