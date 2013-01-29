;pro nrs_bias_removal_gui_define_buttons, buttonInfo
;  envi_define_menu_button, buttonInfo, VALUE = 'Remove bias', $
;    UVALUE = 'Remove Bias', EVENT_PRO = 'nrs_bias_removal_gui', $
;    REF_VALUE = 'NRS', POSITION = 'last', /SEPARATOR
;end

pro nrs_bias_removal_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_bias_removal_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_bias_removal_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_bias_removal_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_bias_removal_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_bias_removal_contentPanel = widget_base(uname = 'nrs_bias_removal_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Remove bias from (temporal) data stack')

  nrs_bias_removal_mainPanel = widget_base(nrs_bias_removal_contentPanel, /frame, /col)

  nrs_bias_removal_refstack = cw_dirfile(nrs_bias_removal_mainPanel $
                , title = 'Reference stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_bias_removal_refstack' $
              )

  nrs_bias_removal_targetstack = cw_dirfile(nrs_bias_removal_mainPanel $
                , title = 'Input target stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_bias_removal_targetstack' $
                , event_pro = 'nrs_bias_removal_handle_target' $
              )

  nrs_bias_removal_output_panel = widget_base(nrs_bias_removal_contentPanel, /frame, /col)
  nrs_bias_removal_outputFile = cw_dirfile(nrs_bias_removal_output_panel, uname = 'nrs_bias_removal_outputFile' $
        , style = 'file' $
        , title = 'Output unbiased stack' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_bias_removal_contentPanel $
                , ok_uname = 'nrs_bias_removal_gobutton', ok_value = 'Go!', ok_tooltip = 'Remove bias' $
                , cancel_uname = 'nrs_bias_removal_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_bias_removal_contentpanel

  ; Initialize stuff

  XManager, 'nrs_bias_removal_gui', nrs_bias_removal_contentPanel, /no_block
end
