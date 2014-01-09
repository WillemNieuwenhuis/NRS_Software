pro nrs_growing_degree_days_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_growing_degree_days_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_growing_degree_days_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_growing_degree_days_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_growing_degree_days_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_growing_degree_days_contentPanel = widget_base(uname = 'nrs_growing_degree_days_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Growing degree days')

  nrs_growing_degree_days_mainPanel = widget_base(nrs_growing_degree_days_contentPanel, /frame, /col)

  nrs_growing_degree_days_group = cw_groupbox(nrs_growing_degree_days_mainPanel, group_title = 'Timeseries')              

  nrs_growing_degree_days_refstack = cw_dirfile(nrs_growing_degree_days_group $
                , title = 'Input timeseries' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_growing_degree_days_refstack' $
                , event_pro = 'nrs_growing_degree_days_handle_input' $
              )

  nrs_growing_degree_days_param_group = cw_groupbox(nrs_growing_degree_days_mainPanel, group_title = 'Parameters')              

  nrs_growing_degree_days_tbase_group = widget_base(nrs_growing_degree_days_param_group, row = 2, xpad = 0)
  nrs_growing_degree_days_tbase = widget_base(nrs_growing_degree_days_tbase_group, /row, xpad = -6)
  nrs_growing_degree_days_tbase_intercept = fsc_inputfield(nrs_growing_degree_days_tbase $
                , uname = 'nrs_growing_degree_days_tbase_intercept' $
                , title = 'T-base intercept' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '0.0' $
                , xsize = text_small_width $
                , event_pro =  'nrs_growing_degree_days_handle_tbase' $
              )
  nrs_growing_degree_days_tbase_slope = fsc_inputfield(nrs_growing_degree_days_tbase $
                , uname = 'nrs_growing_degree_days_tbase_slope' $
                , title = 'T-base slope' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '0.0' $
                , xsize = text_small_width $
                , event_pro =  'nrs_growing_degree_days_handle_tbase' $
              )
  nrs_growing_degree_days_tbase_formula = widget_label(nrs_growing_degree_days_tbase_group $
                , uname = 'nrs_growing_degree_days_tbase_formula' $
                , value = 'Tbase = 0.0' $
                , /align_left $
                , xsize = label_wide_width $
              )

  nrs_growing_degree_days_jerk_panel = widget_base(nrs_growing_degree_days_param_group, /row, /nonexclusive)
  nrs_growing_degree_days_jerk_button = widget_button(nrs_growing_degree_days_jerk_panel $
                , uname = 'nrs_growing_degree_days_jerk_button' $
                , value = 'Calculate GDD jerk' $
                , xsize = label_wide_width $
              )

  nrs_growing_degree_days_spring_panel = widget_base(nrs_growing_degree_days_param_group, /row, /nonexclusive)
  nrs_growing_degree_days_spring_button = widget_button(nrs_growing_degree_days_spring_panel $
                , uname = 'nrs_growing_degree_days_spring_button' $
                , value = 'Calculate onset of spring' $
                , xsize = label_wide_width $
              )

  nrs_growing_degree_days_output_panel = widget_base(nrs_growing_degree_days_contentPanel, /frame, /col)
  nrs_growing_degree_days_outputFile = cw_dirfile(nrs_growing_degree_days_output_panel, uname = 'nrs_growing_degree_days_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_growing_degree_days_contentPanel $
                , ok_uname = 'nrs_growing_degree_days_gobutton', ok_value = 'Go!', ok_tooltip = 'Growing degree days' $
                , cancel_uname = 'nrs_growing_degree_days_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, nrs_growing_degree_days_contentPanel, /realize

  ; Initialize stuff

  ; start the form
  XManager, 'nrs_growing_degree_days_gui', nrs_growing_degree_days_contentPanel, /no_block
end
