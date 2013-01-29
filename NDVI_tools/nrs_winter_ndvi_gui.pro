pro nrs_winter_ndvi_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_winter_ndvi_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_winter_ndvi_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_winter_ndvi_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_winter_ndvi_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_winter_ndvi_contentPanel = widget_base(uname = 'nrs_winter_ndvi_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate Winter NDVI')

  nrs_winter_ndvi_mainPanel = widget_base(nrs_winter_ndvi_contentPanel, /frame, /col)

  nrs_winter_ndvi_refstack = cw_dirfile(nrs_winter_ndvi_mainPanel $
                , title = 'Input NDVI' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_winter_ndvi_refstack' $
                , event_pro = 'nrs_winter_ndvi_handle_input' $
              )

  nrs_winter_ndvi_paramPanel = cw_groupbox(nrs_winter_ndvi_contentPanel, group_title = 'Parameters')              

  nrs_winter_ndvi_ipy = fsc_inputfield(nrs_winter_ndvi_paramPanel $
                , uname = 'nrs_winter_ndvi_ipy' $
                , title = 'Images per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '23' $
                , xsize = num_width $
                , /integer $
                , /all_events $
              )

  nrs_winter_ndvi_start_band = fsc_inputfield(nrs_winter_ndvi_paramPanel $
                , uname = 'nrs_winter_ndvi_start_band' $
                , title = 'First band index' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '17' $
                , xsize = num_width $
                , /integer $
                , /all_events $
              )

  nrs_winter_ndvi_aggr_panel = widget_base(nrs_winter_ndvi_paramPanel, /row)
  nrs_winter_ndvi_aggr_label = widget_label(nrs_winter_ndvi_aggr_panel $
                , value = 'Aggregation method' $
                , xsize = label_width $
              )
  nrs_winter_ndvi_aggr_combo = widget_combobox(nrs_winter_ndvi_aggr_panel $
                , uname = 'nrs_winter_ndvi_aggr_combo' $
                , value = ['Mean', 'Median', 'Min', 'Max'] $
;                , event_pro = 'nrs_aggregate_handle_input' $
              )

  nrs_winter_ndvi_output_panel = widget_base(nrs_winter_ndvi_contentPanel, /frame, /col)
  nrs_winter_ndvi_outputFile = cw_dirfile(nrs_winter_ndvi_output_panel, uname = 'nrs_winter_ndvi_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_winter_ndvi_contentPanel $
                , ok_uname = 'nrs_winter_ndvi_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate winter NDVI' $
                , cancel_uname = 'nrs_winter_ndvi_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_winter_ndvi_contentpanel

  ; Initialize stuff
  widget_control, nrs_winter_ndvi_aggr_combo, set_combobox_select = 3  ; preselect max

  XManager, 'nrs_winter_ndvi_gui', nrs_winter_ndvi_contentPanel, /no_block
end
