pro timesat_gui_event, event
	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	  	widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		Widget_Info(wWidget, FIND_BY_UNAME='tmsat_CancelButton'): begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				widget_control, event.top, /destroy
		end

		Widget_Info(wWidget, FIND_BY_UNAME='tmsat_GoButton'): begin
			if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				tmsat_handleGo, Event
		end
	else:

	endcase

end

pro timesat_gui, event
  compile_opt idl2, logical_predicate
  
	label_width = 75
	label_wide_width = 150
	text_width =  70
	num_width =   15

	tmsat_contentPanel = Widget_Base(GROUP_LEADER = wGroup, UNAME = 'tmsat_contentPanel'  $
		, /col  $
		, TITLE = 'TimeSat')

	tmsat_inputPanel = widget_base(tmsat_contentPanel, /frame, /col)
	tmsat_inputFilePanel = widget_base(tmsat_inputPanel, /row)
	
  tmsat_input_text = cw_dirfile(tmsat_inputPanel $
                , title = 'Input' $
                , style = 'envi' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'tmsat_input_text' $
                , event_pro = 'tmsat_handleBrowseInput' $
              )

	tmsat_InfoText = widget_text(tmsat_inputPanel, /wrap $
			, uname = 'tmsat_InfoText' $
			, ysize = 3 $
			, xsize = text_width $
			, value = 'Note: Assumed is 36 images per year' $
		)

  tmsat_range_panel = cw_groupbox(tmsat_contentPanel, uname = 'tmsat_range_panel', group_title = 'Specify input data range')              
  tmsat_range_min = fsc_inputfield(tmsat_range_panel $
                , uname = 'tmsat_range_min' $
                , title = 'Min' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '-1.0' $
                , xsize = num_width $
                , /all_events $
              )
  tmsat_range_max = fsc_inputfield(tmsat_range_panel $
                , uname = 'tmsat_range_max' $
                , title = 'Max' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '1.0' $
                , xsize = num_width $
                , /all_events $
              )
  
  tmsat_paramPanel = cw_groupbox(tmsat_contentPanel, group_title = 'Timesat parameters')              

  tmsat_SavGolText = fsc_inputfield(tmsat_paramPanel $
                , uname = 'tmsat_SavGolText' $
                , title = 'Savitzky-Golay window' $
                , labelalign = 1 $
                , labelsize = label_wide_width $
                , value = '[1,2,3,4]' $
                , xsize = num_width $
                , /all_events $
              )

	tmsat_Toggles = widget_base(tmsat_paramPanel, /nonexclusive, /col)
	tmsat_UpEnvelCheck = widget_button(tmsat_Toggles, value = 'Force upper envelope', uname = 'tmsat_UpEnvelCheck')
	tmsat_RegFitCheck = widget_button(tmsat_Toggles, value = 'Apply regular TIMESAT fit for the last iteration', uname = 'tmsat_RegFitCheck')

	tmsat_outputPanel = widget_base(tmsat_contentPanel, /frame, /col)
	tmsat_output_text = cw_dirfile(tmsat_outputPanel, uname = 'tmsat_output_text' $
        , style = 'file' $
        , title = 'Output' $
        , xsize = text_width $
        , xtitlesize = label_width $
        )
	
  nrs_gui_createButtonPanel, tmsat_contentPanel $
                , ok_uname = 'tmsat_GoButton', ok_value = 'Go!', ok_tooltip = 'Calculate the NDVI according to TimeSat' $
                , cancel_uname = 'tmsat_CancelButton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

	; Make sure we create the form
	Widget_Control, /REALIZE, tmsat_contentPanel

	; Initialize stuff
	widget_control, tmsat_UpEnvelCheck, /set_button
	widget_control, tmsat_RegFitCheck, /set_button
	widget_control, tmsat_range_panel, sensitiv = 0

;	state.parent = tmsat_contentPanel
;	widget_control, tmsat_contentPanel, set_uvalue = state

	XManager, 'timesat_gui', tmsat_contentPanel, /NO_BLOCK
 end

