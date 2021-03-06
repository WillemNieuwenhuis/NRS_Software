pro nrs_aggregate_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_aggregate_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_aggregate_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_aggregate_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_aggregate_contentPanel = widget_base(uname = 'nrs_aggregate_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Aggregate stack images')

  nrs_aggregate_mainPanel = widget_base(nrs_aggregate_contentPanel, /frame, /col)

  nrs_aggregate_refstack = cw_dirfile(nrs_aggregate_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_aggregate_refstack' $
                , event_pro = 'nrs_aggregate_handle_input' $
              )

  nrs_aggregate_aggr_panel = widget_base(nrs_aggregate_mainPanel, /row)
  nrs_aggregate_aggr_label = widget_label(nrs_aggregate_aggr_panel $
                , value = 'Aggregation method' $
                , xsize = label_width $
              )
  nrs_aggregate_aggr_combo = widget_combobox(nrs_aggregate_aggr_panel $
                , uname = 'nrs_aggregate_aggr_combo' $
                , value = ['All', 'Sum', 'Mean', 'Median', 'Min', 'Max'] $
                , event_pro = 'nrs_aggregate_handle_input' $
              )

  nrs_aggregate_usepert_base = widget_base(nrs_aggregate_mainPanel, title='Exclude outliers', /col, /nonexclusive)
  nrs_aggregate_usepert_toggle = widget_button(nrs_aggregate_usepert_base, uname='nrs_aggregate_usepert_toggle'  $
        , /align_left ,value='Exclude outliers' $
        , event_pro = 'nrs_aggregate_toggle_exoutlier' $
        )

  nrs_aggregate_outlier_base = widget_base(nrs_aggregate_mainPanel, /frame, /col $
    , uname = 'nrs_aggregate_outlier_base' $
    , sensitive = 0 $
    )

  nrs_aggregate_pertval_pert = fsc_inputfield(nrs_aggregate_outlier_base $
    , uname = 'nrs_aggregate_pertval_pert' $
    , title = 'Outlier percentile' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '3' $
    , /int $
    , xsize = text_small_width $
    , unit = ' %' $
    )

  nrs_aggregate_outliers_base = widget_base(nrs_aggregate_outlier_base, title='Report outliers', /col, /nonexclusive)
  nrs_aggregate_outliers_toggle = widget_button(nrs_aggregate_outliers_base, uname='nrs_aggregate_outliers_toggle'  $
    , /align_left ,value='Report outliers' $
    , event_pro = 'nrs_aggregate_toggle_report' $
    )

  nrs_aggregate_start_date = fsc_inputfield(nrs_aggregate_outlier_base $
    , uname = 'nrs_aggregate_start_date' $
    , title = 'Start date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width * 2 $
    , sensitive = 0 $
    , /all_events $
    )

  nrs_aggregate_end_date = fsc_inputfield(nrs_aggregate_outlier_base $
    , uname = 'nrs_aggregate_end_date' $
    , title = 'End date' $
    , labelalign = 1 $
    , labelsize = label_width $
    , unittext = '(dd-mm-yyyy)' $
    , value = '' $
    , xsize = text_small_width * 2 $
    , sensitive = 0 $
    , /all_events $
    )

  nrs_aggregate_output_panel = widget_base(nrs_aggregate_contentPanel, /frame, /col)
  nrs_aggregate_outputFile = cw_dirfile(nrs_aggregate_output_panel, uname = 'nrs_aggregate_outputFile' $
        , style = 'file' $
        , title = 'Output aggregation' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_aggregate_contentPanel $
                , ok_uname = 'nrs_aggregate_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate aggregation' $
                , cancel_uname = 'nrs_aggregate_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_aggregate_contentpanel

  ; Initialize stuff

  XManager, 'nrs_aggregate_gui', nrs_aggregate_contentPanel, /no_block
end
