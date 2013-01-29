pro nrs_ll_grid_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_ll_grid_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_ll_grid_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_ll_grid_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_ll_grid_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_ll_grid_contentPanel = widget_base(uname = 'nrs_ll_grid_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Generate coordinate grid')

  nrs_ll_grid_mainPanel = widget_base(nrs_ll_grid_contentPanel, /frame, /col)

  nrs_ll_grid_input_image = cw_dirfile(nrs_ll_grid_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_ll_grid_input_image' $
                , event_pro = 'nrs_ll_grid_handle_input' $
              )

  nrs_ll_grid_output_panel = widget_base(nrs_ll_grid_contentPanel, /frame, /col)
  
  nrs_ll_grid_latlon_base = widget_base(nrs_ll_grid_output_panel, /nonexclusive, uname = 'nrs_ll_grid_latlon_base')
  nrs_ll_grid_latlon_button = widget_button(nrs_ll_grid_latlon_base $
                , uname = 'nrs_ll_grid_latlon_button' $
                , value = 'Geographic coordinates' $
                , sensitiv = 0 $
              )

  nrs_ll_grid_x_outputFile = cw_dirfile(nrs_ll_grid_output_panel, uname = 'nrs_ll_grid_x_outputFile' $
        , style = 'file' $
        , title = 'Output X-image' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_ll_grid_y_outputFile = cw_dirfile(nrs_ll_grid_output_panel, uname = 'nrs_ll_grid_y_outputFile' $
        , style = 'file' $
        , title = 'Output Y-image' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_ll_grid_contentPanel $
                , ok_uname = 'nrs_ll_grid_gobutton', ok_value = 'Go!', ok_tooltip = 'Normalize indices' $
                , cancel_uname = 'nrs_ll_grid_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_ll_grid_contentpanel

  ; Initialize stuff
;  widget_control, nrs_ll_grid_latlon_button, set_button = 0

  XManager, 'nrs_ll_grid_gui', nrs_ll_grid_contentPanel, /no_block
end
