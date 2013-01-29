; Add a menu item to the NRS menu
pro nrs_stack_stat_gui_define_buttons, buttonInfo
  envi_define_menu_button, buttonInfo, VALUE = 'Calculate stack statistics', $
    UVALUE = 'Calculate stack statistics', EVENT_PRO = 'nrs_stack_stat_gui', $
    REF_VALUE = 'NRS', POSITION = 'last',/SEPARATOR

end

pro nrs_stack_stat_gui_event, event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_stat_CancelButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stack_stat_GoButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_stack_stat_handleGo, Event
    end
  else:

  endcase

end

pro nrs_stack_stat_gui, _EXTRA = Extra
  state = { $
    parent:   long(0) $
  }

  label_width = 130
  label_wide_width = 150
  text_width =  70
  num_width =   15

  nrs_stack_stat_contentPanel = Widget_Base(GROUP_LEADER = wGroup, UNAME = 'nrs_stack_stat_contentPanel'  $
    , /col  $
    , TITLE = 'Stack pooled standard deviation')

  nrs_stack_stat_inputPanel = widget_base(nrs_stack_stat_contentPanel, /frame, /col)
  nrs_stack_stat_inputImage = cw_dirfile(nrs_stack_stat_inputPanel $
                , title = 'Input stack' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_stat_inputImage' $
                , event_pro = 'nrs_stack_stat_handleBrowseInput' $
              )

  nrs_stack_stat_img_py = fsc_inputfield(nrs_stack_stat_inputPanel $
                , uname = 'nrs_stack_stat_img_py' $
                , title = 'NDVI layers per year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '36' $
                , xsize = text_small_width $
                , /integervalue $
                , /all_events $
              )

  nrs_stack_stat_classes = cw_dirfile(nrs_stack_stat_inputPanel $
                , title = 'Input class image' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_stat_classes' $
              )
              
  nrs_stack_stat_outputPanel = widget_base(nrs_stack_stat_contentPanel, /frame, /col)
  nrs_stack_stat_outputTable = cw_dirfile(nrs_stack_stat_outputPanel $
                , title = 'Output table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stack_stat_outputTable' $
              )

  nrs_stack_stat__createButtonPanel, nrs_stack_stat_contentPanel

  widget_control, /realize, nrs_stack_stat_contentpanel
  
  state.parent = nrs_stack_stat_contentpanel
  widget_control, nrs_stack_stat_contentpanel, set_uvalue = state

  XManager, 'nrs_stack_stat_gui', nrs_stack_stat_contentpanel, /no_block
  
end

pro nrs_stack_stat__createButtonPanel, parent
  ; Button panel (OK, Cancel etc)
  Buttonpanel = Widget_Base(parent $
    , /ALIGN_RIGHT $
    , /Row, SPACE = 3 $
    , XPAD = 3 $
    )

  change_detection_GoButton = Widget_Button(Buttonpanel, UNAME='nrs_stack_stat_GoButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Start calculation of pooled standard deviation' ,VALUE='Go!')

  change_detection_CancelButton = Widget_Button(Buttonpanel, UNAME='nrs_stack_stat_CancelButton'  $
    ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
    ,/ALIGN_CENTER $
    ,TOOLTIP='Cancel the operation' ,VALUE='Close')

end
