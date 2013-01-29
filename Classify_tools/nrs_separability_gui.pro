pro nrs_separability_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_separability_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_separability_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_separability_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_separability_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_separability_contentPanel = widget_base(uname = 'nrs_separability_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Separability of classified map')

  nrs_separability_mainPanel = widget_base(nrs_separability_contentPanel, /frame, /col)

  nrs_separability_refstack = cw_dirfile(nrs_separability_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_separability_refstack' $
                , event_pro = 'nrs_separability_handle_input' $
              )

  nrs_separability_classmap = cw_dirfile(nrs_separability_mainPanel $
                , title = 'Input classified image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_separability_classmap' $
                , event_pro = 'nrs_separability_handle_input' $
              )

  nrs_separability_InfoText = widget_text(nrs_separability_mainPanel, /wrap $
      , uname = 'nrs_separability_InfoText' $
      , value = 'Separability result: ---' $
      , ysize = 3 $
      , xsize = text_width $
    )

  nrs_separability_mat_base = widget_base(nrs_separability_mainPanel, /nonexclusive, uname = 'nrs_separability_mat_base')
  nrs_separability_mat_button = widget_button(nrs_separability_mat_base $
                , uname = 'nrs_separability_mat_button' $
                , value = 'Save entire matrix' $
              )

  nrs_separability_output_panel = widget_base(nrs_separability_contentPanel, /frame, /col)
  nrs_separability_outputFile = cw_dirfile(nrs_separability_output_panel, uname = 'nrs_separability_outputFile' $
        , style = 'file' $
        , title = 'Output file' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_separability_contentPanel $
                , ok_uname = 'nrs_separability_gobutton', ok_value = 'Go!', ok_tooltip = 'Separability of classified map' $
                , cancel_uname = 'nrs_separability_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_separability_contentpanel

  ; Initialize stuff

  XManager, 'nrs_separability_gui', nrs_separability_contentPanel, /no_block
end
