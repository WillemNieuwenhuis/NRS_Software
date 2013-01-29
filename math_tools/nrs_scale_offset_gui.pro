pro nrs_scale_offset_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_scale_offset_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_scale_offset_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_scale_offset_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_scale_offset_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_scale_offset_contentPanel = widget_base(uname = 'nrs_scale_offset_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Apply scale and offset')

  nrs_scale_offset_mainPanel = widget_base(nrs_scale_offset_contentPanel, /frame, /col)

  nrs_scale_offset_input_image = cw_dirfile(nrs_scale_offset_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_scale_offset_input_image' $
                , event_pro = 'nrs_scale_offset_handle_input' $
              )

  nrs_scale_offset_scale = fsc_inputfield(nrs_scale_offset_mainPanel $
                , uname = 'nrs_scale_offset_scale' $
                , title = 'Scale' $
                , labelalign = 1 $
                , labelsize = label_width $
                , /float $
                , value = '1.0' $
                , xsize = text_medium_width $
              )

  nrs_scale_offset_offset = fsc_inputfield(nrs_scale_offset_mainPanel $
                , uname = 'nrs_scale_offset_offset' $
                , title = 'Offset' $
                , labelalign = 1 $
                , labelsize = label_width $
                , /float $
                , value = '0.0' $
                , xsize = text_medium_width $
              )
  
  nrs_offset_before_scale_base = widget_base(nrs_scale_offset_mainPanel, /nonexclusive, uname = 'nrs_offset_before_scale_base')
  nrs_offset_before_scale_button = widget_button(nrs_offset_before_scale_base $
                , uname = 'nrs_offset_before_scale_button' $
                , value = 'Offset before scale' $
              )

  nrs_scale_offset_output_panel = widget_base(nrs_scale_offset_contentPanel, /frame, /col)
  nrs_scale_offset_outputFile = cw_dirfile(nrs_scale_offset_output_panel, uname = 'nrs_scale_offset_outputFile' $
        , style = 'file' $
        , title = 'Output image' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_scale_offset_contentPanel $
                , ok_uname = 'nrs_scale_offset_gobutton', ok_value = 'Go!', ok_tooltip = 'Apply scale and offset' $
                , cancel_uname = 'nrs_scale_offset_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_scale_offset_contentpanel

  ; Initialize stuff

  XManager, 'nrs_scale_offset_gui', nrs_scale_offset_contentPanel, /no_block
end
