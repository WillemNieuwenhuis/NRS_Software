pro nrs_gwp_tiff_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_gwp_tiff_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_gwp_tiff_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_gwp_tiff_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_gwp_tiff_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_gwp_tiff_contentPanel = widget_base(uname = 'nrs_gwp_tiff_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Convert Global Water Product RGB')

  nrs_gwp_tiff_mainPanel = widget_base(nrs_gwp_tiff_contentPanel, /frame, /col)

  nrs_gwp_tiff_file = cw_dirfile(nrs_gwp_tiff_mainPanel $
    , title = 'Input GWP tiff' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_gwp_tiff_file' $
    , event_pro = 'nrs_gwp_tiff_handle_input' $
    )

  nrs_gwp_tiff_output_panel = widget_base(nrs_gwp_tiff_contentPanel, /frame, /col)
  nrs_gwp_tiff_outputFile = cw_dirfile(nrs_gwp_tiff_output_panel $
    , uname = 'nrs_gwp_tiff_outputFile' $
    , style = 'file' $
    , title = 'Output tiff file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_gwp_tiff_contentPanel $
    , ok_uname = 'nrs_gwp_tiff_gobutton', ok_value = 'Go!', ok_tooltip = 'Convert Global Water Product RGB' $
    , cancel_uname = 'nrs_gwp_tiff_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_gwp_tiff_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_gwp_tiff_gui', nrs_gwp_tiff_contentPanel, /no_block
end
