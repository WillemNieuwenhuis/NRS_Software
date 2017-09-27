pro nrs_modis_batch_convert_grid_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_modis_batch_convert_grid_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_modis_batch_convert_grid_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_modis_batch_convert_grid_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_modis_batch_convert_grid_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_modis_batch_convert_grid_contentPanel = widget_base(uname = 'nrs_modis_batch_convert_grid_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Batch convert MODIS Grid to ENVI')

  nrs_modis_batch_convert_grid_mainPanel = widget_base(nrs_modis_batch_convert_grid_contentPanel, /frame, /col)

  nrs_modis_batch_convert_grid_infolder = cw_dirfile(nrs_modis_batch_convert_grid_mainPanel $
    , title = 'Input folder' $
    , style = 'directory' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_modis_batch_convert_grid_infolder' $
    )

  nrs_modis_batch_convert_grid_output_panel = widget_base(nrs_modis_batch_convert_grid_contentPanel, /frame, /col)
  nrs_modis_batch_convert_grid_outfolder = cw_dirfile(nrs_modis_batch_convert_grid_output_panel, uname = 'nrs_modis_batch_convert_grid_outfolder' $
    , style = 'directory' $
    , title = 'Output folder' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_modis_batch_convert_grid_contentPanel $
    , ok_uname = 'nrs_modis_batch_convert_grid_gobutton', ok_value = 'Convert', ok_tooltip = 'Batch convert MODIS Grid' $
    , cancel_uname = 'nrs_modis_batch_convert_grid_cancelbutton', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_modis_batch_convert_grid_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_modis_batch_convert_grid_gui', nrs_modis_batch_convert_grid_contentPanel, /no_block
end
