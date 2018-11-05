pro nrs_rename_files_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_rename_files_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_rename_files_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_rename_files_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_rename_files_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15
  
  filelist_width = 60
  filelist_length = 10 ; #files visible

  nrs_rename_files_contentPanel = widget_base(uname = 'nrs_rename_files_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Copy and rename files')

  nrs_rename_files_mainPanel = widget_base(nrs_rename_files_contentPanel, /frame, /col)

  nrs_rename_files_source_folder = cw_dirfile(nrs_rename_files_mainPanel $
      , title = 'Source folder' $
      , style = 'directory' $
      , xsize = text_width $
      , xtitlesize = label_width $
      , uname = 'nrs_rename_files_source_folder' $
      , event_pro = 'nrs_rename_files_fill_filelist' $
    )

  nrs_rename_files_dest_folder = cw_dirfile(nrs_rename_files_mainPanel $
      , title = 'Destination folder' $
      , style = 'directory' $
      , xsize = text_width $
      , xtitlesize = label_width $
      , uname = 'nrs_rename_files_dest_folder' $
    )

  nrs_rename_files_types_panel = widget_base(nrs_rename_files_mainPanel $
    , /row $
    , uname = 'nrs_rename_files_types_panel' $
    )
  nrs_rename_files_types_label = widget_label(nrs_rename_files_types_panel $
    , xsize = label_width $
    , value = 'Select file type' $
    )

  nrs_rename_files_types_combobox = widget_combobox(nrs_rename_files_types_panel $
    , uname = 'nrs_rename_files_types_combobox' $
    , value = ['.*', '.shp', '.tif', '.jpg', '.doc', '.bmp' ] $
    , /editable $ 
    , event_pro = 'nrs_rename_files_fill_filelist' $
    )

  nrs_rename_files_prefix_orig = fsc_inputfield(nrs_rename_files_mainPanel $
    , uname = 'nrs_rename_files_prefix_orig' $
    , title = 'String to replace' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_small_width $
    , value = '' $
    , event_pro = 'nrs_rename_files_fill_renamed_list' $
    )

  nrs_rename_files_prefix_new = fsc_inputfield(nrs_rename_files_mainPanel $
    , uname = 'nrs_rename_files_prefix_new' $
    , title = 'Replacement string' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_small_width $
    , value = '' $
    , event_pro = 'nrs_rename_files_fill_renamed_list' $
    )

  nrs_rename_files_previewPanel = widget_base(nrs_rename_files_contentPanel, /frame, /row)
    
  nrs_rename_files_sources = widget_list(nrs_rename_files_previewPanel $
    , uname = 'nrs_rename_files_sources' $
    , xsize = filelist_width $
    , ysize = filelist_length) 

  nrs_rename_files_destinations = widget_list(nrs_rename_files_previewPanel $
    , uname = 'nrs_rename_files_destinations' $
    , xsize = filelist_width $
    , ysize = filelist_length)

  nrs_gui_createbuttonpanel, nrs_rename_files_contentPanel $
    , ok_uname = 'nrs_rename_files_gobutton', ok_value = 'Go!', ok_enabled = 0, ok_tooltip = 'Rename files' $
    , cancel_uname = 'nrs_rename_files_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_rename_files_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_rename_files_gui', nrs_rename_files_contentPanel, /no_block
end
