pro nrs_harmonic_inv_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_harmonic_inv_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_harmonic_inv_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_harmonic_inv_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_harmonic_inv_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_harmonic_inv_contentPanel = widget_base(uname = 'nrs_harmonic_inv_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Harmonic composition to timeseries')

  nrs_harmonic_inv_mainPanel = widget_base(nrs_harmonic_inv_contentPanel, /frame, /col)

  nrs_harmonic_inv_refstack = cw_dirfile(nrs_harmonic_inv_mainPanel $
                , title = 'Input harmonics' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_harmonic_inv_refstack' $
                , event_pro = 'nrs_harmonic_inv_handle_input' $
              )

  nrs_harmonic_inv_max_harm = fsc_inputfield(nrs_harmonic_inv_mainPanel $
                , uname = 'nrs_harmonic_inv_max_harm' $
                , title = 'Max harmonic' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '3' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_harmonic_inv_years = fsc_inputfield(nrs_harmonic_inv_mainPanel $
                , uname = 'nrs_harmonic_inv_years' $
                , title = 'Number of years' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_harmonic_inv_output_panel = cw_groupbox(nrs_harmonic_inv_contentPanel, group_title = 'Output')              
;  nrs_harmonic_inv_output_panel = widget_base(nrs_harmonic_inv_contentPanel, /frame, /col)
  nrs_harmonic_inv_img_py = fsc_inputfield(nrs_harmonic_inv_output_panel $
                , uname = 'nrs_harmonic_inv_img_py' $
                , title = 'Images per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '23' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_harmonic_inv_outputFile = cw_dirfile(nrs_harmonic_inv_output_panel, uname = 'nrs_harmonic_inv_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_harmonic_inv_contentPanel $
                , ok_uname = 'nrs_harmonic_inv_gobutton', ok_value = 'Go!', ok_tooltip = 'Harmonic composition of timeseries' $
                , cancel_uname = 'nrs_harmonic_inv_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_harmonic_inv_contentpanel

  ; Initialize stuff

  XManager, 'nrs_harmonic_inv_gui', nrs_harmonic_inv_contentPanel, /no_block
end
