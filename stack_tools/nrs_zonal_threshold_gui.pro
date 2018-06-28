pro nrs_zonal_threshold_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_threshold_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_zonal_threshold_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_threshold_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_zonal_threshold_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_zonal_threshold_contentPanel = widget_base(uname = 'nrs_zonal_threshold_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Zonal threshold')

  nrs_zonal_threshold_mainPanel = widget_base(nrs_zonal_threshold_contentPanel, /frame, /col)

  nrs_zonal_threshold_refstack = cw_dirfile(nrs_zonal_threshold_mainPanel $
    , title = 'Input stack' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_threshold_refstack' $
    , event_pro = 'nrs_zonal_threshold_handle_input' $
    )

  nrs_zonal_threshold_classfile = cw_dirfile(nrs_zonal_threshold_mainPanel $
    , title = 'Classified image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_threshold_classfile' $
    )

  nrs_zonal_threshold_step = fsc_inputfield(nrs_zonal_threshold_mainPanel $
    , uname = 'nrs_zonal_threshold_threshold' $
    , title = 'Threshold' $
    , labelalign = 1 $
    , labelsize = label_width $
;    , value = '5' $
    , xsize = text_medium_width $
    )

  nrs_zonal_threshold_ignore = fsc_inputfield(nrs_zonal_threshold_mainPanel $
    , uname = 'nrs_zonal_threshold_ignore' $
    , title = 'Ignore value' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_zonal_threshold_output_panel = widget_base(nrs_zonal_threshold_contentPanel, /frame, /col)

  nrs_zonal_threshold_outputFile = cw_dirfile(nrs_zonal_threshold_output_panel, uname = 'nrs_zonal_threshold_outputFile' $
    , style = 'file' $
    , title = 'Output image' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_zonal_threshold_contentPanel $
    , ok_uname = 'nrs_zonal_threshold_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate zonal threshold' $
    , cancel_uname = 'nrs_zonal_threshold_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_zonal_threshold_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_zonal_threshold_gui', nrs_zonal_threshold_contentPanel, /no_block
end
