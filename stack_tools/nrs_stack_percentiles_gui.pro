pro nrs_stack_percentiles_gui_event, event
  compile_opt idl2, logical_predicate

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_percentiles_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_percentiles_exec, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_percentiles_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_stack_percentiles_gui, event
  compile_opt idl2, logical_predicate

  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 5
  num_width =   15

  nrs_stack_percentiles_contentPanel = widget_base(uname = 'nrs_stack_percentiles_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Stack percentiles')

  nrs_stack_percentiles_mainPanel = widget_base(nrs_stack_percentiles_contentPanel, /frame, /col)

  nrs_stack_percentiles_image = cw_dirfile(nrs_stack_percentiles_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_percentiles_image' $
                , event_pro = 'nrs_stack_percentiles_image_change' $
              )

  nrs_stack_percentiles_percentiles = fsc_inputfield(nrs_stack_percentiles_mainPanel $
    , uname = 'nrs_stack_percentiles_percentiles' $
    , title = 'Percentiles' $
    , labelalign = 1 $
    , labelsize = label_width $
    , value = '50' $
    , xsize = text_medium_width $
    , unit = ' %' $
    )

  nrs_nrs_stack_percentiles_ignore = fsc_inputfield(nrs_stack_percentiles_mainPanel $
    , uname = 'nrs_stack_percentiles_ignore' $
    , title = 'Ignore value' $
    , labelalign = 1 $
    , labelsize = label_width $
    , xsize = text_medium_width $
    )

  nrs_stack_percentiles_output_panel = widget_base(nrs_stack_percentiles_contentPanel, /frame, /col)
  nrs_stack_percentiles_outputFile = cw_dirfile(nrs_stack_percentiles_output_panel $
        , uname = 'nrs_stack_percentiles_outputFile' $
        , style = 'file' $
        , title = 'Output quartiles' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

;  nrs_stack_percentiles_even_label = widget_label(nrs_stack_percentiles_output_panel, /align_left, value = 'On even number of samples:')
;  nrs_stack_percentiles_exact_base = widget_base(nrs_stack_percentiles_output_panel, /exclusive, uname = 'nrs_stack_percentiles_use_folder_base')
;  nrs_stack_percentiles_avgOnEven_button = widget_button(nrs_stack_percentiles_exact_base $
;                , uname = 'nrs_stack_percentiles_avgOnEven_button' $
;                , value = 'Average middle values' $
;              )
;  nrs_stack_percentiles_lowOnEven_button = widget_button(nrs_stack_percentiles_exact_base $
;    , uname = 'nrs_stack_percentiles_lowOnEven_button' $
;    , value = 'Select lower of middle values' $
;    )
;
  nrs_gui_createButtonPanel, nrs_stack_percentiles_contentPanel $
                , ok_uname = 'nrs_stack_percentiles_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate temporal quartiles' $
                , cancel_uname = 'nrs_stack_percentiles_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_stack_percentiles_contentpanel

  ; Initialize stuff
;  widget_control, nrs_stack_percentiles_avgOnEven_button, set_button = 1

  XManager, 'nrs_stack_percentiles_gui', nrs_stack_percentiles_contentPanel, /no_block
end
