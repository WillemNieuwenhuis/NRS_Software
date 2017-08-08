; ENVI 5 extension support
pro nrs_savgol_row_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Savitzky-Golay (by row)', 'nrs_savgol_row_gui';, PATH='Spatial'
end

pro nrs_savgol_row_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_savgol_row_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_savgol_row_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_savgol_row_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_savgol_row_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_savgol_row_contentPanel = widget_base(uname = 'nrs_savgol_row_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Apply Savitzky-Golay filter (by row)')

  nrs_savgol_row_mainPanel = widget_base(nrs_savgol_row_contentPanel, /frame, /col)

  nrs_savgol_row_input_csv = cw_dirfile(nrs_savgol_row_mainPanel $
                , title = 'Input CSV table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_savgol_row_input_csv' $
                , event_pro = 'nrs_savgol_row_handle_input' $
              )

  nrs_savgol_row_group = cw_groupbox(nrs_savgol_row_mainPanel, group_title = 'Savitzky-Golay filter parameters')              
  nrs_savgol_row_width = fsc_inputfield(nrs_savgol_row_group $
                , uname = 'nrs_savgol_row_width' $
                , title = 'Width' $
                , labelalign = 1 $
                , labelsize = label_width $
                , /integer $
                , value = '33' $
                , xsize = text_medium_width $
              )

  nrs_savgol_row_degree = fsc_inputfield(nrs_savgol_row_group $
                , uname = 'nrs_savgol_row_degree' $
                , title = 'Degree' $
                , labelalign = 1 $
                , labelsize = label_width $
                , /integer $
                , value = '4' $
                , xsize = text_medium_width $
              )
  nrs_savgol_row_order_panel = widget_base(nrs_savgol_row_group, /row)
  nrs_savgol_row_datatype_label = widget_label(nrs_savgol_row_order_panel $
                , xsize = label_width $
                , value = 'Order' $
              )
  nrs_savgol_row_order_combo = widget_combobox(nrs_savgol_row_order_panel $
                , uname = 'nrs_savgol_row_order_combo' $
                , value = ['0 (smooth)', '1 (1st derivative)', '2 (2nd derivative)'] $
              )
  
  nrs_savgol_row_output_panel = widget_base(nrs_savgol_row_contentPanel, /frame, /col)
  
  nrs_savgol_row_outputFile = cw_dirfile(nrs_savgol_row_output_panel $
        , uname = 'nrs_savgol_row_outputFile' $
        , style = 'file' $
        , title = 'Output table' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_savgol_row_contentPanel $
                , ok_uname = 'nrs_savgol_row_gobutton', ok_value = 'Go!', ok_tooltip = 'Apply Savitzky-Golay filter' $
                , cancel_uname = 'nrs_savgol_row_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_savgol_row_contentpanel

  ; Initialize stuff

  XManager, 'nrs_savgol_row_gui', nrs_savgol_row_contentPanel, /no_block
end
