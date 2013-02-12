pro nrs_spi_gui_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Standardized Precipitation Index', 'nrs_spi_gui', PATH='Precipitation'
end

pro nrs_spi_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_spi_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_spi_handleOK, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_spi_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_spi_gui, event
  label_width = 100
  label_wide_width = 150
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_spi_contentPanel = widget_base(uname = 'nrs_spi_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'Calculate SPI')

  nrs_spi_mainPanel = widget_base(nrs_spi_contentPanel, /frame, /col)

  nrs_spi_refstack = cw_dirfile(nrs_spi_mainPanel $
                , title = 'Input timeseries' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_spi_refstack' $
                , event_pro = 'nrs_spi_handle_input' $
              )

  nrs_spi_timescales = fsc_inputfield(nrs_spi_mainPanel $
                , uname = 'nrs_spi_timescales' $
                , title = 'Time scales' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1,3,6,12' $
                , xsize = text_small_width $
                , unittext = 'Month(s)' $
              )
              
  nrs_spi_start_year = fsc_inputfield(nrs_spi_mainPanel $
                , uname = 'nrs_spi_start_year' $
                , title = 'Start year' $
                , /integervalue $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
              )
              
  nrs_spi_end_year = fsc_inputfield(nrs_spi_mainPanel $
                , uname = 'nrs_spi_end_year' $
                , title = 'End year' $
                , /integervalue $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
              )
  
  nrs_spi_output_panel = widget_base(nrs_spi_contentPanel, /frame, /col)
  nrs_spi_outputFile = cw_dirfile(nrs_spi_output_panel, uname = 'nrs_spi_outputFile' $
        , style = 'file' $
        , title = 'Output name' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )

  nrs_gui_createButtonPanel, nrs_spi_contentPanel $
                , ok_uname = 'nrs_spi_gobutton', ok_value = 'Go!', ok_tooltip = 'Calculate SPI values from (monthly) timeseries' $
                , cancel_uname = 'nrs_spi_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_spi_contentpanel

  ; Initialize stuff

  XManager, 'nrs_spi_gui', nrs_spi_contentPanel, /no_block
end
