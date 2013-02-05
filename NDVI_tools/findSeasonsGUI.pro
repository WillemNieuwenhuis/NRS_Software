pro findSeasonsGUI_define_buttons, buttonInfo

  envi_define_menu_button, buttonInfo, VALUE = 'Find NDVI seasons', $
    UVALUE = 'Find NDVI seasons', EVENT_PRO = 'findSeasonsGUI_ev', $
    REF_VALUE = 'NRS', POSITION = 'last',/separator

end

pro findSeasonsGUI_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='fndSeason_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='fndSeason_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        fndSeason_handleGo, Event
    end
  else:

  endcase

end

;----------
; GUI entry point
pro findSeasonsGUI_ev, Event
  findSeasonsGUI
end

pro findSeasonsGUI
  label_width = 110
  label_wide_width = 150
  text_width =  65
  num_width =   15

  fndSeason_contentPanel = Widget_Base(GROUP_LEADER = wGroup, UNAME = 'fndSeason_contentPanel'  $
    , /col  $
    , TITLE = 'Find NDVI seasons')

  fndSeason_mainPanel = widget_base(fndSeason_contentPanel, /frame, /col)
  fndSeason_inputImage = cw_dirfile(fndSeason_mainPanel $
                , title = 'Input image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'fndSeason_inputImage' $
                , event_pro = 'fndSeason_handleBrowseInput' $
              )

  fndSeason_datePanel = widget_base(fndSeason_mainPanel, /row)
  fndSeason_startDate = cw_field_ex(fndSeason_datePanel $
                , title = 'Start date (yyyymmdd)' $
                , tsize = label_width $
                , uname = 'fndSeason_startDate' $
                , value = '20100101' $
              )
              
  fndSeason_aggregatePeriod = cw_field_ex(fndSeason_datePanel $
                , title = 'Aggregate period (days)' $
                , tsize = label_width $
                , uname = 'fndSeason_aggregatePeriod' $
                , value = 16 $
              )
              
  fndSeason_Shapefile = cw_dirfile(fndSeason_mainPanel $
                , title = 'Shapefile' $
                , filter = [['*.shp'], ['ESRI shapefiles']] $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'fndSeason_Shapefile' $
              )
  fndSeason_outputPanel = widget_base(fndSeason_contentPanel, /frame, /col)
  fndSeason_outputTable = cw_dirfile(fndSeason_outputPanel $
                , title = 'Output table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'fndSeason_outputTable' $
              )
  
  fndSeason__createButtonPanel, fndSeason_contentPanel

  ; Make sure we create the form
  Widget_Control, /REALIZE, fndSeason_contentPanel

  ; Initialize stuff

  XManager, 'findSeasonsGUI', fndSeason_contentPanel, /NO_BLOCK

end

pro fndSeason__createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  Buttonpanel = Widget_Base(parent $
    , /ALIGN_RIGHT $
    , /Row, SPACE = 3 $
    , XPAD = 3 $
    )

  fndSeason_GoButton = Widget_Button(Buttonpanel, UNAME='fndSeason_GoButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Start processing' ,VALUE='Go!')

  fndSeason_CancelButton = Widget_Button(Buttonpanel, UNAME='fndSeason_CancelButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Cancel the operation' ,VALUE='Close')

end
