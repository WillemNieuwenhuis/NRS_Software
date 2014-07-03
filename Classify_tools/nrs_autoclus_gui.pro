pro nrs_autoclus_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_autoclus_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_autoclus_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_autoclus_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_autoclus_gui, event
  label_width = 120
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_autoclus_contentPanel = widget_base(uname = 'nrs_autoclus_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Automate cluster / separability')

  nrs_autoclus_mainPanel = widget_base(nrs_autoclus_contentPanel, /frame, /col)

  nrs_autoclus_refstack = cw_dirfile(nrs_autoclus_mainPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_autoclus_refstack' $
                , event_pro = 'nrs_autoclus_handle_input' $
              )

  nrs_autoclus_group = cw_groupbox(nrs_autoclus_mainPanel, group_title = 'Cluster parameters')              

  nrs_autoclus_minclasses = fsc_inputfield(nrs_autoclus_group $
                , uname = 'nrs_autoclus_minclasses' $
                , title = 'Nr. of classes (low)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '10' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_autoclus_maxclasses = fsc_inputfield(nrs_autoclus_group $
                , uname = 'nrs_autoclus_maxclasses' $
                , title = 'Nr. of classes (high)' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '15' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_autoclus_iterations = fsc_inputfield(nrs_autoclus_group $
                , uname = 'nrs_autoclus_iterations' $
                , title = 'Iterations' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '50' $
                , /integer $
                , xsize = text_small_width $
              )

  nrs_autoclus_mask_base = widget_base(nrs_autoclus_group, /nonexclusive, uname = 'nrs_autoclus_mask_base')
  nrs_autoclus_mask_button = widget_button(nrs_autoclus_mask_base $
                , uname = 'nrs_autoclus_mask_button' $
                , value = 'Classify zeroes' $
              )

  nrs_autoclus_separ_group = cw_groupbox(nrs_autoclus_mainPanel, group_title = 'Separability parameters')              

  nrs_autoclus_stat_base = widget_base(nrs_autoclus_separ_group, /nonexclusive, uname = 'nrs_autoclus_stat_base', /row)
  nrs_autoclus_stat_button = widget_button(nrs_autoclus_stat_base $
                , uname = 'nrs_autoclus_stat_button' $
                , value = 'Save entire matrix' $
              )

  nrs_autoclus_append_button = widget_button(nrs_autoclus_stat_base $
                , uname = 'nrs_autoclus_append_button' $
                , value = 'Append table output' $
              )

  nrs_autoclus_output_panel = widget_base(nrs_autoclus_contentPanel, /frame, /col)
  nrs_autoclus_outputFile = cw_dirfile(nrs_autoclus_output_panel, uname = 'nrs_autoclus_outputFile' $
        , style = 'file' $
        , title = 'Cluster image basename' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_autoclus_outputTable = cw_dirfile(nrs_autoclus_output_panel, uname = 'nrs_autoclus_outputTable' $
        , style = 'file' $
        , title = 'Separability table' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_autoclus_contentPanel $
                , ok_uname = 'nrs_autoclus_gobutton', ok_value = 'Go!', ok_tooltip = 'Automate cluster / separability' $
                , cancel_uname = 'nrs_autoclus_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_autoclus_contentpanel

  ; Initialize stuff

  XManager, 'nrs_autoclus_gui', nrs_autoclus_contentPanel, /no_block
end
