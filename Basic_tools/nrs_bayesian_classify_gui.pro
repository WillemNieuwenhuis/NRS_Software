pro nrs_bayesian_classify_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_bayesian_classify_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_bayesian_classify_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_bayesian_classify_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_bayesian_classify_gui, event
  compile_opt idl2
  
  label_width = 150
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_bayesian_classify_contentPanel = widget_base(uname = 'nrs_bayesian_classify_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Bayesian classification')

  nrs_bayesian_classify_mainPanel = widget_base(nrs_bayesian_classify_contentPanel, /col, /frame)              
  
  nrs_bayesian_classify_locations = cw_dirfile(nrs_bayesian_classify_mainPanel $
                , title = 'Measurement table' $
                , style = 'file' $
                , filter = [['*.txt;*.csv', '*'], $
                            ['Text tables (txt, csv)', 'All files']] $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_bayesian_classify_locations' $
                , event_pro = 'nrs_bayesian_classify_handle_locations' $
              )

  nrs_bayesian_classify_conditional_folder = cw_dirfile(nrs_bayesian_classify_mainPanel $
                , title = 'Conditional probabilities folder' $
                , style = 'directory' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_bayesian_classify_conditional_folder' $
              )

  nrs_bayesian_classify_prior_table = cw_dirfile(nrs_bayesian_classify_mainPanel $
              , title = 'Prior probabilities table' $
              , style = 'file' $
              , filter = [['*.txt;*.csv', '*'], $
                          ['Text tables (txt, csv)', 'All files']] $
              , xsize = text_width $
              , xtitlesize = label_width $
              , uname = 'nrs_bayesian_classify_prior_table' $
              )

  nrs_bayesian_classify_outputPanel = widget_base(nrs_bayesian_classify_contentPanel, /col, /frame)              
  nrs_bayesian_classify_outtable = cw_dirfile(nrs_bayesian_classify_outputPanel $
                , title = 'Output classification' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_bayesian_classify_outtable' $
              )

  nrs_gui_createButtonPanel, nrs_bayesian_classify_contentPanel $
                , ok_uname = 'nrs_bayesian_classify_gobutton', ok_value = 'Go!', ok_tooltip = 'Bayesian classification' $
                , cancel_uname = 'nrs_bayesian_classify_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_bayesian_classify_contentpanel

  ; Initialize stuff

  XManager, 'nrs_bayesian_classify_gui', nrs_bayesian_classify_contentPanel, /no_block
end
