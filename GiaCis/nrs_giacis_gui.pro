pro nrs_giacis_gui_event, event
	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  	widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='nrs_giacis_CancelButton'): begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				widget_control, event.top, /destroy
		end

		Widget_Info(wWidget, FIND_BY_UNAME='nrs_giacis_GoButton'): begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				nrs_giacis_handleGo, Event
		end
	else:

	endcase

end

pro nrs_giacis_gui, event
  compile_opt idl2, logical_predicate
  
	label_width = 75
	label_wide_width = 150
	text_width =  70
	num_width =   15

	nrs_giacis_contentPanel = Widget_Base(UNAME = 'nrs_giacis_contentPanel'  $
		, /col  $
		, TITLE = 'TimeSat')

	nrs_giacis_inputPanel = widget_base(nrs_giacis_contentPanel, /frame, /col)
	nrs_giacis_inputFilePanel = widget_base(nrs_giacis_inputPanel, /row)
	
  nrs_giacis_input_text = cw_dirfile(nrs_giacis_inputPanel $
                , title = 'Input' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_giacis_input_text' $
                , event_pro = 'nrs_giacis_handleBrowseInput' $
              )

	nrs_giacis_InfoText = widget_text(nrs_giacis_inputPanel, /wrap $
			, uname = 'nrs_giacis_InfoText' $
			, ysize = 3 $
			, xsize = text_width $
			, value = 'Note: Only complete yearly stack of images should be used as an input. ' $
				+ 'Using e.g. a stack like 9 years with 8 months ' $
				+ 'adversely affects the circularity of the data!' $
		)

  nrs_giacis_range_panel = cw_groupbox(nrs_giacis_contentPanel, uname = 'nrs_giacis_range_panel', group_title = 'Specify input data range')              
  nrs_giacis_range_min = fsc_inputfield(nrs_giacis_range_panel $
                , uname = 'nrs_giacis_range_min' $
                , title = 'Min' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '-1.0' $
                , xsize = num_width $
                , /all_events $
              )
  nrs_giacis_range_max = fsc_inputfield(nrs_giacis_range_panel $
                , uname = 'nrs_giacis_range_max' $
                , title = 'Max' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1.0' $
                , xsize = num_width $
                , /all_events $
              )
  
  nrs_giacis_paramPanel = cw_groupbox(nrs_giacis_contentPanel, group_title = 'Timesat parameters')              

  nrs_giacis_SavGolText = fsc_inputfield(nrs_giacis_paramPanel $
                , uname = 'nrs_giacis_SavGolText' $
                , title = 'Savitzky-Golay window' $
                , labelalign = 1 $
                , labelsize = label_wide_width $
                , value = '[1,2,3,4]' $
                , xsize = num_width $
                , /all_events $
              )

	nrs_giacis_Toggles = widget_base(nrs_giacis_paramPanel, /nonexclusive, /col)
	nrs_giacis_UpEnvelCheck = widget_button(nrs_giacis_Toggles, value = 'Force upper envelope', uname = 'nrs_giacis_UpEnvelCheck')
	nrs_giacis_RegFitCheck = widget_button(nrs_giacis_Toggles, value = 'Apply regular TIMESAT fit for the last iteration', uname = 'nrs_giacis_RegFitCheck')

	nrs_giacis_outputPanel = widget_base(nrs_giacis_contentPanel, /frame, /col)
	nrs_giacis_output_text = cw_dirfile(nrs_giacis_outputPanel, uname = 'nrs_giacis_output_text' $
        , style = 'file' $
        , title = 'Output' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )
	
  nrs_gui_createButtonPanel, nrs_giacis_contentPanel $
                , ok_uname = 'nrs_giacis_GoButton', ok_value = 'Go!', ok_tooltip = 'Filter the NDVI according to TimeSat' $
                , cancel_uname = 'nrs_giacis_CancelButton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

	; Make sure we create the form
	Widget_Control, /REALIZE, nrs_giacis_contentPanel

	; Initialize stuff
	widget_control, nrs_giacis_UpEnvelCheck, /set_button
	widget_control, nrs_giacis_RegFitCheck, /set_button
	widget_control, nrs_giacis_range_panel, sensitiv = 0

	XManager, 'nrs_giacis_gui', nrs_giacis_contentPanel, /NO_BLOCK
 end

