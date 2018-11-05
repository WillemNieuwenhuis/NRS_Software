pro nrs_image_linreg_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_image_linreg_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_image_linreg_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_image_linreg_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_image_linreg_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_image_linreg_contentPanel = widget_base(uname = 'nrs_image_linreg_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Temporal linear regression')

  nrs_image_linreg_mainPanel = widget_base(nrs_image_linreg_contentPanel, /frame, /col)

  nrs_image_linreg_refstack = cw_dirfile(nrs_image_linreg_mainPanel $
    , title = 'Input image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_image_linreg_refstack' $
    , event_pro = 'nrs_image_linreg_handle_input' $
    )

  nrs_image_linreg_rmse_panel = widget_base(nrs_image_linreg_mainPanel, /nonexclusive, /col)
  nrs_image_linreg_rmse = widget_button(nrs_image_linreg_rmse_panel $
    , value = 'Calculate RMSE', uname = 'nrs_image_linreg_rmse')

  nrs_image_linreg_output_panel = widget_base(nrs_image_linreg_contentPanel, /frame, /col)
  nrs_image_linreg_outputFile = cw_dirfile(nrs_image_linreg_output_panel, uname = 'nrs_image_linreg_outputFile' $
    , style = 'file' $
    , title = 'Output name' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createButtonPanel, nrs_image_linreg_contentPanel $
    , ok_uname = 'nrs_image_linreg_gobutton', ok_value = 'Go!', ok_tooltip = 'Temporal linear regression' $
    , cancel_uname = 'nrs_image_linreg_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_image_linreg_contentpanel

  ; Initialize stuff

  XManager, 'nrs_image_linreg_gui', nrs_image_linreg_contentPanel, /no_block
end
