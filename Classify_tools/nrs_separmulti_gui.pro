pro nrs_separmulti_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_separmulti_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_separmulti_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_separmulti_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_separmulti_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_separmulti_contentPanel = widget_base(uname = 'nrs_separmulti_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Separability of classified maps')

  nrs_separmulti_mainPanel = widget_base(nrs_separmulti_contentPanel, /frame, /col)

  nrs_separmulti_refstack = cw_dirfile(nrs_separmulti_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_separmulti_refstack' $
                , event_pro = 'nrs_separmulti_handle_input' $
              )

  nrs_separmulti_classmap = cw_dirfile(nrs_separmulti_mainPanel $
                , title = 'Input class imagelist' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_separmulti_classmap' $
                , event_pro = 'nrs_separmulti_handle_input' $
              )

  nrs_separmulti_mat_base = widget_base(nrs_separmulti_mainPanel, /nonexclusive, uname = 'nrs_separmulti_mat_base', /row)
  nrs_separmulti_mat_button = widget_button(nrs_separmulti_mat_base $
                , uname = 'nrs_separmulti_mat_button' $
                , value = 'Save entire matrix' $
              )

  nrs_separmulti_append_button = widget_button(nrs_separmulti_mat_base $
                , uname = 'nrs_separmulti_append_button' $
                , value = 'Append table output' $
              )

  nrs_separmulti_output_panel = widget_base(nrs_separmulti_contentPanel, /frame, /col)
  nrs_separmulti_outputFile = cw_dirfile(nrs_separmulti_output_panel, uname = 'nrs_separmulti_outputFile' $
        , style = 'file' $
        , title = 'Output file' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_separmulti_contentPanel $
                , ok_uname = 'nrs_separmulti_gobutton', ok_value = 'Go!', ok_tooltip = 'Separability of classified map' $
                , cancel_uname = 'nrs_separmulti_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_separmulti_contentpanel

  ; Initialize stuff

  XManager, 'nrs_separmulti_gui', nrs_separmulti_contentPanel, /no_block
end
