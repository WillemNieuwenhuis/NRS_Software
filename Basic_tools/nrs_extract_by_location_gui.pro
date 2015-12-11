pro nrs_extract_by_location_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_extract_by_location_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_extract_by_location_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_extract_by_location_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_extract_by_location_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_extract_by_location_contentPanel = widget_base(uname = 'nrs_extract_by_location_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Extract by location')

  nrs_extract_by_location_mainPanel = widget_base(nrs_extract_by_location_contentPanel, /frame, /col)

  nrs_extract_by_location_points = cw_dirfile(nrs_extract_by_location_mainPanel $
    , title = 'Input shapefile' $
    , style = 'file' $
    , filter = [['*.shp'], $
  ['Shapefiles (shp)']] $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_extract_by_location_points' $
    , event_pro = 'nrs_extract_by_location_handle_points' $
    )

  nrs_extract_by_location_refstack = cw_dirfile(nrs_extract_by_location_mainPanel $
    , title = 'Input image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_extract_by_location_refstack' $
    )

  nrs_extract_by_location_output_panel = widget_base(nrs_extract_by_location_contentPanel, /frame, /col)
  nrs_extract_by_location_outputFile = cw_dirfile(nrs_extract_by_location_output_panel, uname = 'nrs_extract_by_location_outputFile' $
    , style = 'file' $
    , title = 'Output shapefile' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_extract_by_location_contentPanel $
    , ok_uname = 'nrs_extract_by_location_gobutton', ok_value = 'Go!', ok_tooltip = 'Extract by location' $
    , cancel_uname = 'nrs_extract_by_location_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_extract_by_location_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_extract_by_location_gui', nrs_extract_by_location_contentPanel, /no_block
end
