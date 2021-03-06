pro nrs_water_index_batch_gui_extensions_init
  compile_opt IDL2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Batch water index', 'nrs_water_index_batch_gui', PATH='Water indices'
end

pro nrs_water_index_batch_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_water_index_batch_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_water_index_batch_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_water_index_batch_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_water_index_batch_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_water_index_batch_contentPanel = widget_base(uname = 'nrs_water_index_batch_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Batch water index')

  nrs_water_index_batch_mainPanel = widget_base(nrs_water_index_batch_contentPanel, /frame, /col)

  nrs_water_index_batch_use_folder_base = widget_base(nrs_water_index_batch_mainPanel, /nonexclusive, uname = 'nrs_water_index_batch_use_folder_base')
  nrs_water_index_batch_use_folder_button = widget_button(nrs_water_index_batch_use_folder_base $
                , uname = 'nrs_water_index_batch_use_folder_button' $
                , value = 'Use list file' $
                , event_pro = 'nrs_water_index_batch_use_folder_toggle' $
              )

  nrs_water_index_batch_folder = cw_dirfile(nrs_water_index_batch_mainPanel $
                , title = 'Input folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_water_index_batch_folder' $
                , event_pro = 'nrs_water_index_batch_handle_input' $
              )

  nrs_water_index_batch_filelist = cw_dirfile(nrs_water_index_batch_mainPanel $
                , title = 'Input list file' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_water_index_batch_filelist' $
              )

  nrs_water_index_batch_combo_panel = widget_base(nrs_water_index_batch_mainPanel, /row)
  nrs_water_index_combo_batch_label = widget_label(nrs_water_index_batch_combo_panel $
                , value = 'Water-index' $
                , xsize = label_width $
              )

  nrs_water_index_batch_combo = widget_combobox(nrs_water_index_batch_combo_panel $
                , uname = 'nrs_water_index_batch_combo' $
                , value = ['NDWI', 'LSWI1', 'LSWI2', 'TCWI'] $
              )

  nrs_water_index_batch_output_panel = widget_base(nrs_water_index_batch_contentPanel, /frame, /col)
  nrs_water_index_batch_outputFolder = cw_dirfile(nrs_water_index_batch_output_panel $
        , uname = 'nrs_water_index_batch_outputFolder' $
        , style = 'directory' $
        , title = 'Output folder' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_water_index_batch_contentPanel $
                , ok_uname = 'nrs_water_index_batch_gobutton', ok_value = 'Go!', ok_tooltip = 'Batch calculate water index' $
                , cancel_uname = 'nrs_water_index_batch_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_water_index_batch_contentpanel

  ; Initialize stuff
  widget_control, nrs_water_index_batch_folder, sensitive = 1
  widget_control, nrs_water_index_batch_filelist, sensitive = 0

  XManager, 'nrs_water_index_batch_gui', nrs_water_index_batch_contentPanel, /no_block
end
