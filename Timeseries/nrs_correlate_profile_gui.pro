pro nrs_correlate_profile_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_correlate_profile_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_correlate_profile_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_correlate_profile_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_correlate_profile_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_correlate_profile_contentPanel = widget_base(uname = 'nrs_correlate_profile_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Timeseries profile correlation')

  nrs_correlate_profile_mainPanel = widget_base(nrs_correlate_profile_contentPanel, /frame, /col)

  nrs_correlate_profile_group = cw_groupbox(nrs_correlate_profile_mainPanel, group_title = 'Timeseries')              

  nrs_correlate_profile_refstack = cw_dirfile(nrs_correlate_profile_group $
                , title = 'Input timeseries' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_correlate_profile_refstack' $
                , event_pro = 'nrs_correlate_profile_handle_input' $
              )

  nrs_correlate_profile_table = cw_dirfile(nrs_correlate_profile_group $
                , title = 'Input profile' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_correlate_profile_table' $
              )

  nrs_correlate_profile_start_date = fsc_inputfield(nrs_correlate_profile_group $
                , uname = 'nrs_correlate_profile_start_date' $
                , title = 'Start date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , event_pro = 'nrs_correlate_profile_handle_input' $
              )

  nrs_correlate_profile_end_date = fsc_inputfield(nrs_correlate_profile_group $
                , uname = 'nrs_correlate_profile_end_date' $
                , title = 'End date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , event_pro = 'nrs_correlate_profile_handle_input' $
              )

  void = widget_base(nrs_correlate_profile_group, /row)
  nrs_correlate_profile_input_period_label = widget_label(void $
                , uname = 'nrs_correlate_profile_input_period_label' $
                , xsize = label_wide_width $
                , value = 'Input period: --' $
              )
  
  nrs_correlate_profile_param_group = cw_groupbox(nrs_correlate_profile_mainPanel, group_title = 'Parameters')              

  nrs_correlate_profile_NAN_panel = widget_base(nrs_correlate_profile_param_group, /row, /nonexclusive)
  nrs_correlate_profile_NAN_button = widget_button(nrs_correlate_profile_NAN_panel $
                , uname = 'nrs_correlate_profile_NAN_button' $
                , value = 'Ignore missing data' $
                , xsize = label_width $
;                , event_pro = 'nrs_correlate_profile_toggle_NAN_ignore' $
              )

  nrs_correlate_profile_paramrow_panel = widget_base(nrs_correlate_profile_param_group, /row)
  nrs_correlate_profile_paramcol1_panel = widget_base(nrs_correlate_profile_paramrow_panel, /col, xpad = 0)

  nrs_correlate_profile_from_date = fsc_inputfield(nrs_correlate_profile_paramcol1_panel $
                , uname = 'nrs_correlate_profile_from_date' $
                , title = 'From date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)  <-->' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , xpad = 0 $
                , uvalue = 'from_date' $
                , event_pro = 'nrs_correlate_profile_handle_from_to' $
              )

  nrs_correlate_profile_to_date = fsc_inputfield(nrs_correlate_profile_paramcol1_panel $
                , uname = 'nrs_correlate_profile_to_date' $
                , title = 'To date' $
                , labelalign = 1 $
                , labelsize = label_width $
                , unittext = '(dd-mm-yyyy)  <-->' $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , xpad = 0 $
                , uvalue = 'to_date' $
                , event_pro = 'nrs_correlate_profile_handle_from_to' $
              )

  nrs_correlate_profile_paramcol2_panel = widget_base(nrs_correlate_profile_paramrow_panel, /col)
  nrs_correlate_profile_from_band = fsc_inputfield(nrs_correlate_profile_paramcol2_panel $
                , uname = 'nrs_correlate_profile_from_band' $
                , title = 'From band' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , /integer $
                , uvalue = 'from_band' $
                , event_pro = 'nrs_correlate_profile_handle_from_to' $
              )

  nrs_correlate_profile_to_band = fsc_inputfield(nrs_correlate_profile_paramcol2_panel $
                , uname = 'nrs_correlate_profile_to_band' $
                , title = 'To band' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
                , /integer $
                , uvalue = 'to_band' $
                , event_pro = 'nrs_correlate_profile_handle_from_to' $
              )

  nrs_correlate_profile_output_panel = widget_base(nrs_correlate_profile_contentPanel, /frame, /col)
  nrs_correlate_profile_outputFile = cw_dirfile(nrs_correlate_profile_output_panel, uname = 'nrs_correlate_profile_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_correlate_profile_contentPanel $
                , ok_uname = 'nrs_correlate_profile_gobutton', ok_value = 'Go!', ok_tooltip = 'Timeseries profile correlation' $
                , cancel_uname = 'nrs_correlate_profile_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, nrs_correlate_profile_contentPanel, /realize $
                , set_uvalue = { fromdate : nrs_correlate_profile_from_date $
                               , todate : nrs_correlate_profile_to_date $
                               , fromband : nrs_correlate_profile_from_band $
                               , toband : nrs_correlate_profile_to_band $
                 }

  ; Initialize stuff
  widget_control, nrs_correlate_profile_NAN_button, /set_button

  ; start the form
  XManager, 'nrs_correlate_profile_gui', nrs_correlate_profile_contentPanel, /no_block
end
