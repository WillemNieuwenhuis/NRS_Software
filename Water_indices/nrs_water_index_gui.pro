pro nrs_water_index_gui_extensions_init
  compile_opt IDL2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Water indices', 'nrs_water_index_gui', PATH='Water indices'
end

pro nrs_water_index_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_water_index_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_water_index_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_water_index_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_water_index_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_water_index_contentPanel = widget_base(uname = 'nrs_water_index_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Water indices')

  nrs_water_index_mainPanel = widget_base(nrs_water_index_contentPanel, /frame, /col)

  nrs_water_index_refstack = cw_dirfile(nrs_water_index_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_water_index_refstack' $
                , event_pro = 'nrs_water_index_handle_input' $
              )

  nrs_water_index_combo_panel = widget_base(nrs_water_index_mainPanel, /row)
  nrs_water_index_combo_label = widget_label(nrs_water_index_combo_panel $
                , value = 'Water-index' $
                , xsize = label_width $
              )

  nrs_water_index_combo = widget_combobox(nrs_water_index_combo_panel $
                , uname = 'nrs_water_index_combo' $
                , value = ['NDWI', 'LSWI1', 'LSWI2', 'TCWI'] $
                , event_pro = 'nrs_water_index_handle_input' $
              )

  nrs_water_index_output_panel = widget_base(nrs_water_index_contentPanel, /frame, /col)
  nrs_water_index_outputFile = cw_dirfile(nrs_water_index_output_panel, uname = 'nrs_water_index_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_water_index_contentPanel $
                , ok_uname = 'nrs_water_index_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate water index' $
                , cancel_uname = 'nrs_water_index_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_water_index_contentpanel

  ; Initialize stuff
  

  ; start the form
  XManager, 'nrs_water_index_gui', nrs_water_index_contentPanel, /no_block
end
