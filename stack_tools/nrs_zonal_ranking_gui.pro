pro nrs_zonal_ranking_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_ranking_gobutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_zonal_ranking_handleok, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='nrs_zonal_ranking_cancelbutton'): begin
      if( tag_names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
  endcase

end

pro nrs_zonal_ranking_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  text_medium_width = 10
  num_width =   15

  nrs_zonal_ranking_contentPanel = widget_base(uname = 'nrs_zonal_ranking_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Zonal ranking')

  nrs_zonal_ranking_mainPanel = widget_base(nrs_zonal_ranking_contentPanel, /frame, /col)

  nrs_zonal_ranking_refstack = cw_dirfile(nrs_zonal_ranking_mainPanel $
    , title = 'Input stack' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_ranking_refstack' $
    , event_pro = 'nrs_zonal_ranking_handle_input' $
    )

  nrs_zonal_ranking_ignore = fsc_inputfield(nrs_zonal_ranking_mainPanel $
    , uname = 'nrs_zonal_ranking_ignore' $
    , title = 'Ignore value' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_zonal_ranking_classfile = cw_dirfile(nrs_zonal_ranking_mainPanel $
    , title = 'Classified image' $
    , style = 'envi' $
    , xsize = text_width $
    , xtitlesize = label_width $
    , uname = 'nrs_zonal_ranking_classfile' $
    , event_pro = 'nrs_zonal_ranking_handle_class_input' $
    )

  nrs_zonal_ranking_exclude_clzero_base = widget_base(nrs_zonal_ranking_mainPanel, /nonexclusive, uname = 'nrs_zonal_ranking_exclude_clzero_base')
  nrs_zonal_ranking_exclude_clzero_button = widget_button(nrs_zonal_ranking_exclude_clzero_base $
    , uname = 'nrs_zonal_ranking_exclude_clzero_button' $
    , value = 'Exclude class zero' $
    )

  nrs_zonal_ranking_step = fsc_inputfield(nrs_zonal_ranking_mainPanel $
    , uname = 'nrs_zonal_ranking_step' $
    , title = 'Rank step' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '5' $
    , xsize = text_medium_width $
    , unit = ' %' $
    )

  nrs_zonal_ranking_zfactor_base = widget_base(nrs_zonal_ranking_mainPanel, /nonexclusive, uname = 'nrs_zonal_ranking_zfactor_base')
  nrs_zonal_ranking_zfactor_button = widget_button(nrs_zonal_ranking_zfactor_base $
    , uname = 'nrs_zonal_ranking_zfactor_button' $
    , value = 'Calculate Z-factor' $
    )

  nrs_zonal_ranking_output_panel = widget_base(nrs_zonal_ranking_contentPanel, /frame, /col)

  nrs_zonal_ranking_outputFile = cw_dirfile(nrs_zonal_ranking_output_panel, uname = 'nrs_zonal_ranking_outputFile' $
    , style = 'file' $
    , title = 'Output ranking' $
    , xsize = text_width $
    , xtitlesize = label_width $
    )

  nrs_gui_createbuttonpanel, nrs_zonal_ranking_contentPanel $
    , ok_uname = 'nrs_zonal_ranking_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate zonal percentiles' $
    , cancel_uname = 'nrs_zonal_ranking_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_zonal_ranking_contentpanel

  ; Initialize stuff

  xmanager, 'nrs_zonal_ranking_gui', nrs_zonal_ranking_contentPanel, /no_block
end
