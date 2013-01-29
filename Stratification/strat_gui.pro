pro amapp_define_buttons, button_info
	envi_define_menu_button, button_info, value = 'NDVI Stratification', $
		uvalue = 'View NDVI Stratification', event_pro = 'amapp', $
		ref_value = 'NRS', position = 'last', /separator
end

pro amapp_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='BrowseStratInputButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handle_strat_BrowseInput, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseDEMButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handle_strat_BrowseDEM, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseStratFileButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleSaveStratification, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CloseStratButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handle_strat_Close, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CalcStratButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handle_strat_Calculate, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='SavePictureButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleSavePicture, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowsePictureFileButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseSavePicture, Event
    end
    Widget_Info(wWidget, FIND_BY_uname='browseTableFileButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseHeightTable, Event
    end
    Widget_Info(wWidget, FIND_BY_uname='lutTypeCombobox'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        handleChangeLutType, Event
    end
    else:
  endcase

end

pro amapp, event
	statevalue={parent:         long(0), $
			inputName:			long(0), $
			inputID:			long(0), $		; the file ID of the original image
			inputDEMname:		long(0), $
			inputDEMID:			long(0), $
            stratoutput:		long(0), $		; the file to store the stratification image
            pictureOutput:		long(0), $
            HeightStep:			long(0), $		; the text element containg the height step value
            heightTable:		long(0), $		; filename of the height table
            matrix:				ptr_new(), $	; stratification matrix
            height_data:		ptr_new(), $	; the height data for the graphs
            header:				string(''), $
            dem_min:			float(0.0), $
            dem_max:			float(0.0), $
            draw_height:		long(0), $		; total height of the plot area in pixels
            draw_width:			long(0), $		; total width of the plot area in pixels
            legend_width:		long(0), $		; total width of the legend plot area in pixels
            draw_scale:			float(0.0), $

            wSEED:0L}

	; make sure we use the envi libraries
;	envi_batch

	; constants
	text_size = 450
	padding = 20

	; dimensions of plot area
	scale = 1.5
	tot_height = 300 * scale
	tot_width = 600 * scale
	tot_leg_width = 50 * scale

	; Form definition
	Strat_form = Widget_Base( UNAME='Stratification_form', /col  $
		,TITLE='AMaPP')

	; Selection of the input image data
	inputPanel = widget_base(Strat_form, /col, frame=1)

	inputSeriesPanel = widget_base(inputPanel, /row)
	InputSeriesLabel = Widget_Label(inputSeriesPanel, UNAME='InputSeriesLabel'  $
		,/ALIGN_LEFT ,VALUE='Input time series:')

	InputNameText = Widget_Text(inputSeriesPanel, UNAME='InputNameText' $
		, SCR_XSIZE=text_size   $
		,/EDITABLE ,/ALL_EVENTS)

	BrowseInputButton = Widget_Button(inputSeriesPanel, UNAME='BrowseStratInputButton' $
		,/ALIGN_CENTER ,VALUE='...')

	inputDEMPanel = widget_base(inputPanel, /row)
	InputDEMLabel = Widget_Label(inputDEMPanel, UNAME='InputDEMLabel'  $
		,/ALIGN_LEFT ,VALUE='Input DEM:')

	InputDEMText = Widget_Text(inputDEMPanel, UNAME='InputDEMText' $
		, SCR_XSIZE=text_size   $
		,/EDITABLE ,/ALL_EVENTS)

	BrowseDEMButton = Widget_Button(inputDEMPanel, UNAME='BrowseDEMButton' $
		,/ALIGN_CENTER ,VALUE='...')

	; Selection of the stratification parameter(s)
	OptionsPanel = Widget_Base(Strat_form, UNAME='OptionsPanel' ,FRAME=1  $
		,TITLE='IDL', /col)

	DetailsPane = widget_base(OptionsPanel, /col)

	DetailsLabel = Widget_Label(DetailsPane, UNAME='WaveletLabel'  $
		, /ALIGN_LEFT, VALUE='Stratification options')

	HeightStepPanel = widget_base(DetailsPane, /row $
		, XPAD=padding $
		)

	HeightStepLabel = Widget_Label(HeightStepPanel, UNAME='HeightStepLabel'  $
		,/ALIGN_LEFT ,VALUE='Height step:')

	HeightStepText = Widget_Text(HeightStepPanel, UNAME='HeightStepText' $
		, SCR_XSIZE=50   $
		, value = '100' $
		,/EDITABLE ,/ALL_EVENTS)

	HeightStepUnitLabel = Widget_Label(HeightStepPanel, UNAME='HeightStepUnitLabel'  $
		,/ALIGN_LEFT ,VALUE=' m')

	HeightLineLabel = Widget_Label(HeightStepPanel, UNAME='HeightLineLabel'  $
		, scr_xsize = 75 $
		,/ALIGN_LEFT ,VALUE='')

	tableBasePanel = widget_base(Strat_form, frame=1, /col)

	tableTitleLabel = Widget_Label(tableBasePanel, UNAME='tableTitleLabel'  $
		, /ALIGN_LEFT $
		, VALUE='Height information' $
	)

	tablePanel = widget_base(tableBasePanel, uname='tablePanel', /row $
		, XPAD=padding $
	)

	tableLabel = Widget_Label(tablePanel, UNAME='tableLabel'  $
		, /ALIGN_LEFT $
		, VALUE='Input height table:' $
	)

	tableText = widget_text(tablePanel, uname='tableText' $
		, SCR_XSIZE=text_size  $
		,/EDITABLE $
	)

	browseTableFileButton = Widget_Button(tablePanel  $
		, uname='browseTableFileButton' $
		, /ALIGN_CENTER $
		, value='...' $
		, tooltip='Open text table containing panda heights through the year' $
	)

	OutputPanel = Widget_Base(Strat_form, UNAME='OutputPanel' ,FRAME=1  $
		, /col $
		,TITLE='IDL')

	OutputLabel = Widget_Label(OutputPanel, UNAME='OutputLabel'  $
		,/ALIGN_LEFT ,VALUE='Output')

	StratDetailsPanel = widget_base(OutputPanel, /row $
		, XPAD=padding $
	)

	StratOutputLabel = Widget_Label(StratDetailsPanel, UNAME='StratOutputLabel'  $
		,/ALIGN_LEFT ,VALUE='Stratification name:')

	StratOutputText = Widget_Text(StratDetailsPanel, UNAME='StratOutputText'  $
		, SCR_XSIZE=text_size  $
		,/EDITABLE)

	BrowseStratFileButton = Widget_Button(StratDetailsPanel,  $
		UNAME='BrowseStratFileButton' $
		,/ALIGN_CENTER ,VALUE='...', tooltip='Save stratification file')

	pictureDetailsPanel = widget_base(OutputPanel, /row $
		, XPAD=padding $
	)

	pictureOutputLabel = Widget_Label(pictureDetailsPanel, UNAME='pictureOutputLabel'  $
		,/ALIGN_LEFT ,VALUE='Picture name:')

	pictureOutputText = Widget_Text(pictureDetailsPanel, UNAME='pictureOutputText'  $
		, SCR_XSIZE=text_size  $
		,/EDITABLE)

	BrowsePictureFileButton = Widget_Button(pictureDetailsPanel,  $
		UNAME='BrowsePictureFileButton' $
		,/ALIGN_CENTER ,VALUE='...', tooltip='Save drawing as picture file')

	calcButtonPanel = widget_base(OutputPanel, /align_right, /row $
	)

	SavePictureButton = Widget_Button(calcButtonPanel, UNAME='SavePictureButton' $
		,VALUE='Save', Tooltip='Saev drawing as picture file')

	CalcButton = Widget_Button(calcButtonPanel, UNAME='CalcStratButton' $
		,VALUE='Calculate', Tooltip='Calculate the stratification')

	DrawPanel = Widget_Base(Strat_form, UNAME='DrawPanel' ,FRAME=1  $
		, /col $
		,TITLE='IDL')

	panes = widget_base(drawPanel, uname='panes', /row, title = 'IDL')

	strat_draw = widget_draw(panes, UNAME='StratDraw' $
		, X_SCROLL_SIZE = tot_width $
		, Y_SCROLL_SIZE = tot_height $
		, xsize = tot_width $
		, ysize = tot_height $
		, /APP_SCROLL $
		, RETAIN = 2 $
	)

	bottom_panel = widget_base(drawPanel, /row)

	lutTypePanel = widget_base(bottom_panel, /row $
	)

	lutDummyLabel = widget_label(lutTypePanel $
		, xsize = padding $
		, value = '' $
	)

	lutTypeLabel = widget_label(lutTypePanel, uname='lutTypeLabel' $
		, value = 'Select color table:' $
	)

	lutTypeCombobox = widget_combobox(lutTypePanel, uname='lutTypeCombobox' $
		, value = ['Rainbow', 'Black/White'] $
	)

	; align text boxes
	xsize = widget_info(InputSeriesLabel, string_size = 'Input time series:')
	clabel_xsize = widget_info(InputDEMLabel, string_size = 'Input DEM:')
	cryst_xsize = padding + widget_info(HeightStepLabel, string_size = 'Height step:')
	icryst_xsize = padding + widget_info(StratOutputLabel, string_size = 'Stratification name:')
	tbl_xsize = padding + widget_info(tableLabel, string_size = 'Input height table:')
	lut_xsize = widget_info(lutTypeLabel, string_size = 'Select color table:')
	pic_xsize = widget_info(pictureOutputLabel, string_size = 'Picture name:')
	label_size = xsize[0]
	if clabel_xsize[0] gt label_size then label_size = clabel_xsize[0]
	if cryst_xsize[0] gt label_size then label_size = cryst_xsize[0]
	if icryst_xsize[0] gt label_size then label_size = icryst_xsize[0]
	if tbl_xsize[0] gt label_size then label_size = tbl_xsize[0]
	if pic_xsize[0] gt label_size then label_size = pic_xsize[0]
	widget_control, InputSeriesLabel, xsize = label_size
	widget_control, InputDEMLabel, xsize = label_size
	widget_control, HeightStepLabel, xsize = label_size - padding
	widget_control, StratOutputLabel, xsize = label_size - padding
	widget_control, tableLabel, xsize = label_size - padding
	widget_control, lutTypeLabel, xsize = label_size - padding
	widget_control, pictureOutputLabel, xsize = label_size - padding

	closeButtonPanel = widget_base(Strat_form, /align_right, /col)
	CloseButton = Widget_Button(closeButtonpanel, UNAME='CloseStratButton'  $
		,TOOLTIP='Close the window' ,VALUE='Close')

	statevalue.inputName=inputNameText
	statevalue.inputDEMName=inputDEMText
	statevalue.stratoutput=StratOutputText
	statevalue.pictureOutput = pictureOutputText
	statevalue.HeightStep = HeightStepText
	statevalue.heightTable = tableText
	statevalue.draw_height = tot_height / scale
	statevalue.draw_width = tot_width / scale
	statevalue.legend_width = tot_leg_width / scale
	statevalue.draw_scale = scale

	; save the current form state ...
	widget_control, Strat_form, set_uvalue = statevalue

	;  ... and create the form
	Widget_Control, /REALIZE, Strat_form

	XManager, 'amapp', Strat_form, /NO_BLOCK

end
