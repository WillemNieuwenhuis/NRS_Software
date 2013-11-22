pro nrs_model_perf_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_model_perf_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_model_perf_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_model_perf_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_model_perf_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_model_perf_contentPanel = widget_base(uname = 'nrs_model_perf_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Model performance indices')

  nrs_model_perf_mainPanel = widget_base(nrs_model_perf_contentPanel, /frame, /col)

  nrs_model_perf_obs = cw_dirfile(nrs_model_perf_mainPanel $
                , title = 'Input observations' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_model_perf_obs' $
                , event_pro = 'nrs_model_perf_handle_input' $
              )

  nrs_model_perf_pred = cw_dirfile(nrs_model_perf_mainPanel $
                , title = 'Input predictions' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_model_perf_pred' $
              )

  nrs_model_perf_num_bands = fsc_inputfield(nrs_model_perf_mainPanel $
                , uname = 'nrs_model_perf_num_bands' $
                , title = 'Number of bands' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '' $
                , /integer $
                , xsize = text_small_width $
                , sensitive = 0 $
              )

  nrs_model_perf_btn_group = cw_groupbox(nrs_model_perf_mainPanel, group_title = 'Output indices')
  nrs_model_perf_indices_base = widget_base(nrs_model_perf_btn_group, /nonexclusive, uname = 'nrs_model_perf_indices_base')
  nrs_model_perf_rmse_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_rmse_button' $
                , value = 'RMSE' $
              )
  nrs_model_perf_ef_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_ef_button' $
                , value = 'R2 / EF (model efficiency)' $
              )
  nrs_model_perf_cd_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_cd_button' $
                , value = 'CD (Coefficient of determination)' $
              )
  nrs_model_perf_e_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_e_button' $
                , value = 'E (relative error)' $
              )
  nrs_model_perf_mre_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_mre_button' $
                , value = 'MRE (mean relative error)' $
              )
  nrs_model_perf_md_button = widget_button(nrs_model_perf_indices_base $
                , uname = 'nrs_model_perf_md_button' $
                , value = 'MD (Mean difference)' $
              )

  nrs_model_perf_output_panel = widget_base(nrs_model_perf_contentPanel, /frame, /col)
  nrs_model_perf_outputFile = cw_dirfile(nrs_model_perf_output_panel, uname = 'nrs_model_perf_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_model_perf_contentPanel $
                , ok_uname = 'nrs_model_perf_gobutton', ok_value = 'Go!', ok_tooltip = 'Model performance indices' $
                , cancel_uname = 'nrs_model_perf_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_model_perf_contentpanel

  ; Initialize stuff
  

  ; show the dialog
  XManager, 'nrs_model_perf_gui', nrs_model_perf_contentPanel, /no_block
end
