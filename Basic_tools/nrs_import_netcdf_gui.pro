pro nrs_import_netcdf_gui_extensions_init
  compile_opt IDL2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Import netCDF files', 'nrs_import_netcdf_gui', PATH='Basic tools'
end

pro nrs_import_netcdf_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_import_netcdf_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_import_netcdf_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_import_netcdf_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_import_netcdf_gui, event
  compile_opt idl2
  
  label_width = 110
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_import_netcdf_contentPanel = widget_base(uname = 'nrs_import_netcdf_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Import netCDF')

  nrs_import_netcdf_mainPanel = widget_base(nrs_import_netcdf_contentPanel, /col, /frame)              
  
  nrs_import_netcdf_input_folder = cw_dirfile(nrs_import_netcdf_mainPanel $
                , title = 'Input folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_import_netcdf_input_folder' $
              )

  nrs_import_netcdf_mask = fsc_inputfield(nrs_import_netcdf_mainPanel $
                , uname = 'nrs_import_netcdf_mask' $
                , title = 'File mask' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '*.nc' $
                , xsize = text_small_width $
              )

  nrs_import_netcdf_outputPanel = widget_base(nrs_import_netcdf_contentPanel, /col, /frame)              
  nrs_import_netcdf_outfolder = cw_dirfile(nrs_import_netcdf_outputPanel $
                , title = 'Output folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_import_netcdf_outfolder' $
              )

  nrs_gui_createButtonPanel, nrs_import_netcdf_contentPanel $
                , ok_uname = 'nrs_import_netcdf_gobutton', ok_value = 'Go!', ok_tooltip = 'Import netCDF' $
                , cancel_uname = 'nrs_import_netcdf_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_import_netcdf_contentpanel

  ; Initialize stuff

  XManager, 'nrs_import_netcdf_gui', nrs_import_netcdf_contentPanel, /no_block
end
