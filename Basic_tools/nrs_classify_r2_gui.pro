pro nrs_classify_r2_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_classify_r2_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_classify_r2_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_classify_r2_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_classify_r2_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_classify_r2_contentPanel = widget_base(uname = 'nrs_classify_r2_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Classify R2')

  nrs_classify_r2_mainPanel = widget_base(nrs_classify_r2_contentPanel, /frame, /col)

  nrs_classify_r2_refstack = cw_dirfile(nrs_classify_r2_mainPanel $
                , title = 'Input R2' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_classify_r2_refstack' $
                , event_pro = 'nrs_classify_r2_handle_input' $
              )

  nrs_classify_r2_output_panel = widget_base(nrs_classify_r2_contentPanel, /frame, /col)
  nrs_classify_r2_outputFile = cw_dirfile(nrs_classify_r2_output_panel, uname = 'nrs_classify_r2_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_classify_r2_contentPanel $
                , ok_uname = 'nrs_classify_r2_gobutton', ok_value = 'Go!', ok_tooltip = 'Classify R2 values' $
                , cancel_uname = 'nrs_classify_r2_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_classify_r2_contentpanel

  ; Initialize stuff

  XManager, 'nrs_classify_r2_gui', nrs_classify_r2_contentPanel, /no_block
end
