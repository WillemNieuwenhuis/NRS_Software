pro nrs_harmonic_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_harmonic_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_harmonic_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_harmonic_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_harmonic_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_harmonic_contentPanel = widget_base(uname = 'nrs_harmonic_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Harmonic analysis on timeseries')

  nrs_harmonic_mainPanel = widget_base(nrs_harmonic_contentPanel, /frame, /col)

  nrs_harmonic_refstack = cw_dirfile(nrs_harmonic_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_harmonic_refstack' $
                , event_pro = 'nrs_harmonic_handle_input' $
              )

  nrs_harmonic_img_py = fsc_inputfield(nrs_harmonic_mainPanel $
                , uname = 'nrs_harmonic_img_py' $
                , title = 'Nr. of layers per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '23' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_harmonic_max_harm = fsc_inputfield(nrs_harmonic_mainPanel $
                , uname = 'nrs_harmonic_max_harm' $
                , title = 'Max harmonic' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '3' $
                , /integer $
                , xsize = text_small_width $
                , event_pro = 'nrs_harmonic_max_change' $
              )

  nrs_harmonic_degrees_base = widget_base(nrs_harmonic_mainPanel, /nonexclusive, uname = 'nrs_harmonic_degrees_base')
  nrs_harmonic_degrees_button = widget_button(nrs_harmonic_degrees_base $
                , uname = 'nrs_harmonic_degrees_button' $
                , value = 'Phase in degrees' $
              )

  nrs_harmonic_InfoText = widget_text(nrs_harmonic_mainPanel, /wrap $
      , uname = 'nrs_harmonic_InfoText' $
      , ysize = 3 $
      , xsize = text_width $
    )

  nrs_harmonic_output_panel = widget_base(nrs_harmonic_contentPanel, /frame, /col)
  nrs_harmonic_outputFile = cw_dirfile(nrs_harmonic_output_panel, uname = 'nrs_harmonic_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_harmonic_contentPanel $
                , ok_uname = 'nrs_harmonic_gobutton', ok_value = 'Go!', ok_tooltip = 'Harmonic analysis on timeseries' $
                , cancel_uname = 'nrs_harmonic_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_harmonic_contentpanel

  ; Initialize stuff
  event = {id : nrs_harmonic_max_harm, top : nrs_harmonic_contentpanel}
  nrs_harmonic_max_change, event

  XManager, 'nrs_harmonic_gui', nrs_harmonic_contentPanel, /no_block
end
