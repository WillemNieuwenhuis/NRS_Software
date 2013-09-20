pro nrs_rainfall_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Consecutive dry/wet days', 'nrs_rainfall_gui', PATH='Precipitation'
end

pro nrs_rainfall_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rainfall_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_rainfall_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_rainfall_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_rainfall_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_rainfall_contentPanel = widget_base(uname = 'nrs_rainfall_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate rainfall/dry period')

  nrs_rainfall_mainPanel = widget_base(nrs_rainfall_contentPanel, /frame, /col)

  nrs_rainfall_refstack = cw_dirfile(nrs_rainfall_mainPanel $
                , title = 'Input time series' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_rainfall_refstack' $
                , event_pro = 'nrs_rainfall_handle_input' $
              )

  nrs_rainfall_drylimit = fsc_inputfield(nrs_rainfall_mainPanel $
                , uname = 'nrs_rainfall_drylimit' $
                , title = 'Dry limit' $
                , /integervalue $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , value = 1 $
                , unittext = 'mm' $
              )
              
;  nrs_rainfall_start_date = fsc_inputfield(nrs_rainfall_mainPanel $
;                , uname = 'nrs_rainfall_start_date' $
;                , title = 'Start date' $
;                , labelalign = 1 $
;                , labelsize = label_width $
;                , xsize = text_small_width $
;                , unittext = '(dd-mm-yyyy)' $
;              )
;              
;  nrs_rainfall_end_date = fsc_inputfield(nrs_rainfall_mainPanel $
;                , uname = 'nrs_rainfall_end_date' $
;                , title = 'End date' $
;                , labelalign = 1 $
;                , labelsize = label_width $
;                , xsize = text_small_width $
;                , unittext = '(dd-mm-yyyy)' $
;              )
;  
  nrs_rainfall_output_panel = widget_base(nrs_rainfall_contentPanel, /frame, /col)
  nrs_rainfall_outputFile = cw_dirfile(nrs_rainfall_output_panel, uname = 'nrs_rainfall_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_rainfall_contentPanel $
                , ok_uname = 'nrs_rainfall_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate rainfall period from timeseries' $
                , cancel_uname = 'nrs_rainfall_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_rainfall_contentpanel

  ; Initialize stuff

  XManager, 'nrs_rainfall_gui', nrs_rainfall_contentPanel, /no_block
end
