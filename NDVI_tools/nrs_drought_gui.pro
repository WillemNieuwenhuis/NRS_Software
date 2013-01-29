pro nrs_drought_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_drought_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_drought_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_drought_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_drought_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_drought_contentPanel = widget_base(uname = 'nrs_drought_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate drought index')

  nrs_drought_mainPanel = widget_base(nrs_drought_contentPanel, /frame, /col)

  nrs_drought_VCI_image = cw_dirfile(nrs_drought_mainPanel $
                , title = 'VCI image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_drought_VCI_image' $
                , event_pro = 'nrs_drought_handle_input' $
              )

  nrs_drought_TCI_image = cw_dirfile(nrs_drought_mainPanel $
                , title = 'TCI image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_drought_TCI_image' $
              )

  nrs_drought_output_panel = widget_base(nrs_drought_contentPanel, /frame, /col)
  nrs_drought_outputFile = cw_dirfile(nrs_drought_output_panel, uname = 'nrs_drought_outputFile' $
        , style = 'file' $
        , title = 'Output file' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_drought_contentPanel $
                , ok_uname = 'nrs_drought_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate drought index' $
                , cancel_uname = 'nrs_drought_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_drought_contentpanel

  ; Initialize stuff

  XManager, 'nrs_drought_gui', nrs_drought_contentPanel, /no_block
end
