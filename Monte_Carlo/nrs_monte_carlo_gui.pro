pro nrs_monte_carlo_gui_define_buttons, buttoninfo
  envi_define_menu_button, buttoninfo, value = 'Monte Carlo Simulation', $
    uvalue = 'Monte Carlo Simulation', event_pro = 'nrs_monte_carlo_gui', $
    ref_value = 'NRS', position = 'last'

end

pro nrs_monte_carlo_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_monte_carlo_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_monte_carlo_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_monte_carlo_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_monte_carlo_gui, event
  label_width = 120
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_monte_carlo_contentPanel = widget_base(uname = 'nrs_monte_carlo_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Monte Carlo simulation')

  nrs_monte_carlo_mainPanel = widget_base(nrs_monte_carlo_contentPanel, /frame, /col)

  nrs_monte_carlo_input_table = cw_dirfile(nrs_monte_carlo_mainPanel $
                , title = 'Input table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_monte_carlo_input_table' $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables', 'All files']] $
                , event_pro = 'nrs_monte_carlo_handle_input' $
              )

  nrs_monte_carlo_simulation_group = cw_groupbox(nrs_monte_carlo_mainPanel, group_title = 'Simulation')              

  nrs_monte_carlo_iteration = fsc_inputfield(nrs_monte_carlo_simulation_group $
                , uname = 'nrs_monte_carlo_iteration' $
                , title = 'Number of iterations' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1000' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_seg_select = fsc_inputfield(nrs_monte_carlo_simulation_group $
                , uname = 'nrs_monte_carlo_seg_select' $
                , title = 'S''' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , unittext = ' %' $
                , /all_events $
              )

  nrs_monte_carlo_acc_select = fsc_inputfield(nrs_monte_carlo_simulation_group $
                , uname = 'nrs_monte_carlo_acc_select' $
                , title = 'A''' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , unittext = ' %' $
                , /all_events $
              )

  nrs_monte_carlo_iter_output_base = widget_base(nrs_monte_carlo_simulation_group, /nonexclusive, uname = 'nrs_model_perf_indices_base')
  nrs_monte_carlo_iter_output_button = widget_button(nrs_monte_carlo_iter_output_base $
                , uname = 'nrs_monte_carlo_iter_output_button' $
                , value = 'Output individual iteration results' $
              )

  nrs_monte_carlo_params_group = cw_groupbox(nrs_monte_carlo_mainPanel, group_title = 'Simulation ranges')
                
  nrs_monte_carlo_carbon_field = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_carbon_field' $
                , title = 'Carbon (field-based)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_carbon_field_wrong = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_carbon_field_wrong' $
                , title = 'Carbon (field-based wrong)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_cpa_well = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_cpa_well' $
                , title = 'CPA (well)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_cpa_poor = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_cpa_poor' $
                , title = 'CPA (poor)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_height_well = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_height_well' $
                , title = 'Height (well)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_height_poor = fsc_inputfield(nrs_monte_carlo_params_group $
                , uname = 'nrs_monte_carlo_height_poor' $
                , title = 'Height (poor)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , xsize = text_small_width $
                , /all_events $
              )

  nrs_monte_carlo_output_panel = widget_base(nrs_monte_carlo_contentPanel, /frame, /col)
  nrs_monte_carlo_outputFile = cw_dirfile(nrs_monte_carlo_output_panel $
        , uname = 'nrs_monte_carlo_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_monte_carlo_contentPanel $
                , ok_uname = 'nrs_monte_carlo_gobutton', ok_value = 'Go!', ok_tooltip = 'Start simulation' $
                , cancel_uname = 'nrs_monte_carlo_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_monte_carlo_contentpanel

  ; Initialize stuff

  XManager, 'nrs_monte_carlo_gui', nrs_monte_carlo_contentPanel, /no_block
end
