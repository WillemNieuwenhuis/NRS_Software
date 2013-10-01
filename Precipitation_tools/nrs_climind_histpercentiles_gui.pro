pro nrs_climind_perc_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Historical 95th percentile', 'nrs_climind_perc_gui', PATH='Climate indices'
end

pro nrs_climind_perc_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_perc_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_climind_perc_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_climind_perc_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_climind_perc_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_climind_perc_contentPanel = widget_base(uname = 'nrs_climind_perc_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Historical 95th percentile')

  nrs_climind_perc_mainPanel = widget_base(nrs_climind_perc_contentPanel, /frame, /col)

  nrs_climind_perc_refstack = cw_dirfile(nrs_climind_perc_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_climind_perc_refstack' $
                , event_pro = 'nrs_climind_perc_handle_input' $
              )

  nrs_climind_perc_base = widget_base(nrs_climind_perc_mainPanel, /nonexclusive, uname = 'nrs_climind_perc_base')
  nrs_climind_perc_button = widget_button(nrs_climind_perc_base $
                , uname = 'nrs_climind_perc_button' $
                , value = 'Use out-of-base estimation' $
              )

  nrs_climind_perc_output_panel = widget_base(nrs_climind_perc_contentPanel, /frame, /col)
  nrs_climind_perc_outputFile = cw_dirfile(nrs_climind_perc_output_panel, uname = 'nrs_climind_perc_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_climind_perc_contentPanel $
                , ok_uname = 'nrs_climind_perc_gobutton', ok_value = 'Go!', ok_tooltip = 'Historical 95th percentile' $
                , cancel_uname = 'nrs_climind_perc_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_climind_perc_contentpanel

  ; Initialize stuff

  XManager, 'nrs_climind_perc_gui', nrs_climind_perc_contentPanel, /no_block
end
