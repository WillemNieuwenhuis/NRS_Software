pro nrs_average_spectrum_gui_event, event
  compile_opt idl2
  
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_average_spectrum_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_average_spectrum_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_average_spectrum_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_average_spectrum_gui, event
  compile_opt idl2
  
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_average_spectrum_contentPanel = widget_base(uname = 'nrs_average_spectrum_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Average spectrum')

  nrs_average_spectrum_mainPanel = widget_base(nrs_average_spectrum_contentPanel, /frame, /col)

  nrs_average_spectrum_image_folder = cw_dirfile(nrs_average_spectrum_mainPanel $
                , title = 'Input image folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_average_spectrum_image_folder' $
              )

  nrs_average_spectrum_shape_folder = cw_dirfile(nrs_average_spectrum_mainPanel $
                , title = 'Input shape folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_average_spectrum_shape_folder' $
              )

  nrs_average_spectrum_kernel_panel = widget_base(nrs_average_spectrum_mainPanel, /row)
  nrs_average_spectrum_kernel_label = widget_label(nrs_average_spectrum_kernel_panel $
                , value = 'Kernel size' $
                , xsize = label_width $
              )
  nrs_average_spectrum_kernel_combo = widget_combobox(nrs_average_spectrum_kernel_panel $
                , uname = 'nrs_average_spectrum_kernel_combo' $
                , value = ['3', '5', '7'] $
              )

  nrs_average_spectrum_toggle_panel = widget_base(nrs_average_spectrum_mainPanel $
                , title = 'Use majority threshold' $
                , /col $
                , /nonexclusive $
              )

  nrs_average_spectrum_toggle = widget_button(nrs_average_spectrum_toggle_panel $
                , uname = 'nrs_average_spectrum_toggle'  $
                , /align_left $
                , value = 'Use majority threshold' $
                , event_pro = 'nrs_handle_average_spectrum_toggle' $
              )

  nrs_average_spectrum_threshold_panel = widget_base(nrs_average_spectrum_mainPanel $
                , uname = 'nrs_average_spectrum_threshold_panel' $
                , /row $
                , sensitiv = 0 $
              )
  nrs_average_spectrum_threshold = fsc_inputfield(nrs_average_spectrum_threshold_panel $
                , uname = 'nrs_average_spectrum_threshold' $
                , title = 'Threshold' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , /integer $
                , value = '0' $ 
                , unittext = ' (%)' $
              )

  nrs_average_spectrum_output_panel = widget_base(nrs_average_spectrum_contentPanel, /frame, /col)
  nrs_average_spectrum_outputFile = cw_dirfile(nrs_average_spectrum_output_panel, uname = 'nrs_average_spectrum_outputFile' $
        , style = 'file' $
        , title = 'Output table' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_average_spectrum_contentPanel $
                , ok_uname = 'nrs_average_spectrum_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate average spectra' $
                , cancel_uname = 'nrs_average_spectrum_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_average_spectrum_contentpanel

  ; Initialize stuff
  widget_control, nrs_average_spectrum_kernel_combo, set_combobox_select = 2 

  XManager, 'nrs_average_spectrum_gui', nrs_average_spectrum_contentPanel, /no_block
end
