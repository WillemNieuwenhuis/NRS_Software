pro nrs_import_netcdf_file_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_import_netcdf_file_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_import_netcdf_file_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_import_netcdf_file_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_import_netcdf_file_gui, event
  compile_opt idl2
  
  label_width = 110
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_import_netcdf_file_contentPanel = widget_base(uname = 'nrs_import_netcdf_file_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Import netCDF')

  nrs_import_netcdf_file_mainPanel = widget_base(nrs_import_netcdf_file_contentPanel, /col, /frame)              
  
  nrs_import_netcdf_file_input_file = cw_dirfile(nrs_import_netcdf_file_mainPanel $
                , title = 'Input NetCDF file' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_import_netcdf_file_input_file' $
                , event_pro = 'nrs_import_netcdf_file_handle_input' $
              )

  nrs_import_netcdf_file_listPanel = widget_base(nrs_import_netcdf_file_mainPanel, /row)
  dummy = widget_base(nrs_import_netcdf_file_listPanel, /col, xpad = 0)
  nrs_import_netcdf_file_list_label = widget_label(dummy $ ;nrs_import_netcdf_file_listPanel $
    , value = 'Variables' $
    , xsize = label_width $
    , /align_top $
    )

  nrs_import_netcdf_file_list = widget_list(nrs_import_netcdf_file_listPanel $
    , uname = 'nrs_import_netcdf_file_list' $
    , /multiple $
    , xsize = text_width $
    , ysize = 8 $
    )
    
  nrs_import_netcdf_file_dates_panel = widget_base(nrs_import_netcdf_file_mainPanel, /col, sens = 0 $
    , uname = 'nrs_import_netcdf_file_dates_panel' $
    )

  nrs_import_netcdf_file_start_date = fsc_inputfield(nrs_import_netcdf_file_dates_panel $
    , uname = 'nrs_import_netcdf_file_start_date' $
    , title = 'Start date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width * 3 $
    , /all_events $
    )

  nrs_import_netcdf_file_end_date = fsc_inputfield(nrs_import_netcdf_file_dates_panel $
    , uname = 'nrs_import_netcdf_file_end_date' $
    , title = 'End date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width * 3 $
    , /all_events $
    )

  nrs_import_netcdf_file_outputPanel = widget_base(nrs_import_netcdf_file_contentPanel, /col, /frame)              
  nrs_import_netcdf_file_outfile = cw_dirfile(nrs_import_netcdf_file_outputPanel $
                , title = 'Output base filename' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_import_netcdf_file_outfile' $
              )

  nrs_gui_createButtonPanel, nrs_import_netcdf_file_contentPanel $
                , ok_uname = 'nrs_import_netcdf_file_gobutton', ok_value = 'Go!', ok_tooltip = 'Import netCDF' $
                , cancel_uname = 'nrs_import_netcdf_file_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_import_netcdf_file_contentpanel

  ; Initialize stuff

  XManager, 'nrs_import_netcdf_file_gui', nrs_import_netcdf_file_contentPanel, /no_block
end
