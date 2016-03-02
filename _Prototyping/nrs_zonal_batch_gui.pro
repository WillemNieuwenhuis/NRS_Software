pro nrs_zonal_batch_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_batch_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_zonal_batch_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_batch_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_zonal_batch_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_zonal_batch_contentPanel = widget_base(uname = 'nrs_zonal_batch_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Batch calculate zonal fraction')

  nrs_zonal_batch_mainPanel = widget_base(nrs_zonal_batch_contentPanel, /frame, /col)

  nrs_zonal_batch_folder = cw_dirfile(nrs_zonal_batch_mainPanel $
    , title = 'Input folder' $
    , style = 'directory' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_batch_folder' $
    , event_pro = 'nrs_zonal_batch_handle_input' $
    )

  nrs_zonal_batch_extension = fsc_inputfield(nrs_zonal_batch_mainPanel $
    , uname = 'nrs_zonal_batch_extension' $
    , title = 'Image extension' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = 'dat' $
    , xsize = num_width $
    , /all_events $
    )

  nrs_zonal_batch_shapefile = cw_dirfile(nrs_zonal_batch_mainPanel $
    , title = 'Shapefile' $
    , filter = [['*.shp', '*'], $
        ['Shapefiles', 'All files']] $
        , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_batch_shapefile' $
    , event_pro = 'nrs_zonal_batch_handle_shapefile' $
    )

  nrs_zonal_batch_attribute_panel = widget_base(nrs_zonal_batch_mainPanel $
    , /row $
    , uname = 'nrs_zonal_batch_attribute_panel' $
    )
  nrs_zonal_batch_attribute_label = widget_label(nrs_zonal_batch_attribute_panel $
    , xsize = label_width $
    , value = 'Select attribute' $
    )

  nrs_zonal_batch_attribute_combobox = widget_combobox(nrs_zonal_batch_attribute_panel $
    , uname = 'nrs_zonal_batch_attribute_combobox' $
    , value = [ ] $
    , /editable $
    )

  nrs_zonal_batch_output_panel = widget_base(nrs_zonal_batch_contentPanel, /frame, /col)
  nrs_zonal_batch_outputFolder = cw_dirfile(nrs_zonal_batch_output_panel $
    , uname = 'nrs_zonal_batch_outputFolder' $
    , style = 'directory' $
    , title = 'Output folder' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_zonal_batch_use_table_base = widget_base(nrs_zonal_batch_output_panel, /nonexclusive, uname = 'nrs_zonal_batch_use_table_base')
  nrs_zonal_batch_use_table_button = widget_button(nrs_zonal_batch_use_table_base $
    , uname = 'nrs_zonal_batch_use_table_button' $
    , value = 'Output to table' $
    , event_pro = 'nrs_zonal_batch_use_table_toggle' $
    )

  nrs_zonal_batch_outtable = cw_dirfile(nrs_zonal_batch_output_panel $
    , title = 'Output table' $
    , style = 'file' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_batch_outtable' $
    )

  nrs_gui_createbuttonpanel, nrs_zonal_batch_contentPanel $
    , ok_uname = 'nrs_zonal_batch_gobutton', ok_value = 'Go!', ok_tooltip = 'Batch calculate zonal fraction' $
    , cancel_uname = 'nrs_zonal_batch_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_zonal_batch_contentpanel

  ; Initialize stuff
  widget_control, nrs_zonal_batch_folder, sensitive = 1
  widget_control, nrs_zonal_batch_outtable, sensitive = 0

  xmanager, 'nrs_zonal_batch_gui', nrs_zonal_batch_contentPanel, /no_block
end
