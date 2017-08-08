pro nrs_filter_cooccurence_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_filter_cooccurence_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_filter_cooccurence_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_filter_cooccurence_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_filter_cooccurence_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_filter_cooccurence_contentPanel = widget_base(uname = 'nrs_filter_cooccurence_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Texture co-occurrence')

  nrs_filter_cooccurence_mainPanel = widget_base(nrs_filter_cooccurence_contentPanel, /frame, /col)

  nrs_filter_cooccurence_refstack = cw_dirfile(nrs_filter_cooccurence_mainPanel $
    , title = 'Input image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_filter_cooccurence_refstack' $
    )

  nrs_filter_cooccurence_points = cw_dirfile(nrs_filter_cooccurence_mainPanel $
    , title = 'Input shapefile' $
    , style = 'file' $
    , filter = [['*.shp'], $
    ['Shapefiles (shp)']] $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_filter_cooccurence_points' $
    , event_pro = 'nrs_filter_cooccurence_handle_points' $
    )

  nrs_filter_cooccurence_kernel = fsc_inputfield(nrs_filter_cooccurence_mainPanel $
    , uname = 'nrs_filter_cooccurence_kernel' $
    , title = 'Kernel size' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '3' $
    , /integer $
    , xsize = text_small_width $
    )

  nrs_filter_cooccurence_output_panel = widget_base(nrs_filter_cooccurence_contentPanel, /frame, /col)
  nrs_filter_cooccurence_outputFile = cw_dirfile(nrs_filter_cooccurence_output_panel, uname = 'nrs_filter_cooccurence_outputFile' $
    , style = 'file' $
    , title = 'Output shapefile' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_filter_cooccurence_contentPanel $
    , ok_uname = 'nrs_filter_cooccurence_gobutton', ok_tooltip = 'Extract by location' $
    , cancel_uname = 'nrs_filter_cooccurence_cancelbutton', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_filter_cooccurence_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_filter_cooccurence_gui', nrs_filter_cooccurence_contentPanel, /no_block
end
