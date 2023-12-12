pro prospect_gui_event, Event
  	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      	widget_info(Event.id, /tree_root) : event.id)

  	wWidget =  Event.top

	widget_control, event.top, get_uvalue = state

	; Handle the checkboxes on the wavelength page first
	switch wTarget of
		state.plot_r_forest:
		state.plot_r_soil:
		state.plot_r_under:
		state.plot_r_c_inf:
		state.plot_r_leaf:
		state.plot_t_leaf:
		state.plot_C:
		state.plot_G:
		state.plot_t_s:
		state.plot_t_o: begin
			handleRunCommand, Event
			return
		end
	endswitch

  	case wTarget of
   	Widget_Info(wWidget, FIND_BY_UNAME='SoilSpectrumContentBrowseButton'): begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
			handleBrowseSoilSpectrum, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='inform_aboutButton'): begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	inform_handleAbout, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='ResetButton'): begin
		if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	handleReset, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CancelButton'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	widget_control, Event.top, /destroy
    end
    Widget_Info(wWidget, FIND_BY_UNAME='SoilSpectrumDefaultButton'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	handleDefaultSoilSpectrum, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='inform_MSpectrumCheckButton'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	inform_handleMeasuredSpectrumCheck, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='reflectSelectComboBox'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        	inform_handleUpdateAngleGraph, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='viewSelectCombobox'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        	inform_handleUpdateAngleGraph, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wavelengthSelectList'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' )then $
        	inform_handleUpdateAngleGraph, Event
    end


    ; Sensitivity (forward mode)
    Widget_Info(wWidget, FIND_BY_UNAME='sensParameterCombobox'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        	inform_handle_sensbox, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='sensReflectSelectComboBox'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        	inform_handleUpdateSensitivityGraph, Event
    end

    Widget_Info(wWidget, FIND_BY_UNAME='sensParameterRangeStepsText'): begin
      	if( strmid(Tag_Names(Event, /STRUCTURE_NAME), 0, 11) eq 'WIDGET_TEXT' )then $
        	inform_handleUpdateSensitivityGraph, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='graphTab'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_TAB' )then $
        	inform_handleMainTabChange, Event
    end

	; Sensitivity (inverse mode)
    Widget_Info(wWidget, FIND_BY_UNAME='invSensRunButton'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	inform_handleInverseSens, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='refSpectrumBrowseButton'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        	inform_handleBrowseRefSpec, Event
    end

    else:

  	endcase

end

pro prospect_gui, event
	statevalue = {                       $
			parent:             long(0), $
			leafStructure:		long(0), $
			leafChlorophyl:		long(0), $
			leafEqWater:		long(0), $
			leafDryMatter:		long(0), $
			canopyHot:			long(0), $
			canopyScale:		long(0), $
			canopyLAI:			long(0), $
			canopyLAIU:			long(0), $
			canopyStemDensity:	long(0), $
			canopyTreeHeight:	long(0), $
			canopyCrownDiam:	long(0), $
			canopyAvgLeafAngle:	long(0), $
			externalTetaS:		long(0), $
			externalTetaO:		long(0), $
			externalPhi:		long(0), $
			externalSkyl:		long(0), $
			soilspectrum_input:	long(0), $   ; name of the soil spectrum
			SoilSpectrumPanel: 	long(0), $
			UseDefaultSoil:		long(0), $
			draw_pane:			long(0), $
			plot_r_forest:		long(0), $
			plot_r_soil:		long(0), $
			plot_r_under:		long(0), $
			plot_r_c_inf:		long(0), $
			plot_r_leaf:		long(0), $
			plot_t_leaf:		long(0), $
			plot_C:				long(0), $
			plot_G:				long(0), $
			plot_CO:			long(0), $
			plot_t_s:			long(0), $
			plot_t_o:			long(0), $

			print_CO:			long(0), $
			print_LAIc:			long(0), $

			measureSpectrumChk:	long(0), $	; checkbox to enable measured spectrum selection
			measuredSpectrum:	long(0), $	; text box with the name of the file containing the measured spectrum
			measuredPanel:		long(0), $	; panel with the selection text and button

			ShowMeasured:		long(0), $	; 0 = don't show; 1 = show
			measuredLoaded:		long(0), $	; 0 = not loaded; 1 = loaded
			measSpectrum:		ptr_new(), $	; array containing the loaded measured spectrum

			; Model TAB 2
			draw_anglePane:		long(0), $
			select_AngleCB:		long(0), $
			wavelengthSelectList: 	long(0), $
			reflectSelectComboBox: 	long(0), $

            wSEED:              0L       $
    }
    color_draws = { $
    	drawForestColor:			long(0), $
    	drawSoilColor:				long(0), $
    	drawUnderstoreyColor:		long(0), $
    	drawInfCanopyColor:			long(0), $
    	drawLeafRefColor:			long(0), $
    	drawLeafTransColor:			long(0), $
    	drawCrownSColor:			long(0), $
    	drawCrownOColor:			long(0), $
    	drawGroundColor:			long(0), $
    	inform_drawMeasureColor:	long(0)  $
    }

	Resolve_Routine, 'prospect_gui_eventcb', /COMPILE_FULL_FILE  ; Load event callback routines

	ContentPanel = Widget_Base(GROUP_LEADER = wGroup, UNAME = 'ContentPanel'  $
		, /col  $
		, TITLE = 'InforM_Prospect')

	statevalue.parent = ContentPanel	; to have access to the root of the form
										; used for search formelements by uname

	SubContentPanel = Widget_Base(ContentPanel, UNAME = 'SubContentPanel'  $
		, col=2  $
		, TITLE = 'Forward model')

	; Define  Model user interface
	createModelPage, SubContentPanel, statevalue, color_draws, graphTab

	; The button panel
	inform_createButtonPanel, ContentPanel

	; save the current form state ...
	widget_control, ContentPanel, set_uvalue = statevalue

	; Make sure we create the form
	Widget_Control, /REALIZE, ContentPanel

	; Draw initial state
	init_draw_model_tab, statevalue, color_draws

	init_draw_sens_tab, ContentPanel

	XManager, 'prospect_gui', ContentPanel, /NO_BLOCK

end

pro inform_createButtonPanel, parent
	; Button panel (OK, Cancel etc)
	Buttonpanel = Widget_Base(parent, UNAME = 'ButtonPanel'  $
		, /ALIGN_RIGHT $
		, /Row, SPACE = 3 $
		, XPAD = 3 $
		)

	inform_aboutButton = Widget_Button(Buttonpanel, UNAME='inform_aboutButton'  $
		,SCR_XSIZE=50 ,SCR_YSIZE=22  $
		,/ALIGN_CENTER $
		,TOOLTIP='Background information' ,VALUE='About')

	ResetButton = Widget_Button(Buttonpanel, UNAME='ResetButton'  $
		,SCR_XSIZE=50 ,SCR_YSIZE=22  $
		,/ALIGN_CENTER $
		,TOOLTIP='Reset parameters to default' ,VALUE='Defaults')

	CancelButton = Widget_Button(Buttonpanel, UNAME='CancelButton'  $
		,SCR_XSIZE=50 ,SCR_YSIZE=22  $
		,/ALIGN_CENTER $
		,TOOLTIP='Cancel the operation' ,VALUE='Close')

end

pro createModelPage, SubContentPanel, statevalue, color_draws, graphTab
	; Left side: Input parameters and sliders
	inform_createInputParamPage, SubContentPanel, statevalue

	; Right side: draw windows and draw options
	FieldContentPanel = Widget_Base(SubContentPanel, UNAME = 'FieldContentPanel'  $
		, /col  $
		, TITLE = 'FieldContentPanel')

	; Define model tabular pages
	graphTab = widget_tab(FieldContentPanel, uname = 'graphTab' $
		)

	; Tab 1: Wavelength against reflectance
	inform_createWavelengthPage, graphTab, statevalue, color_draws

	; Tab 2: Angle against reflectance
	inform_createAnglePage, graphTab, statevalue

	; Tab 3: Sensitivity analysis (forward)
	inform_createSensitivityPage, graphTab, statevalue

	inform_createInverseSensitivityPage, graphTab, statevalue
end

; Input parameters and sliders
pro inform_createInputParamPage, parent, statevalue
	parLabelSize = 40

	MainProspectPanel = Widget_Base(parent, UNAME = 'MainProspectPanel'  $
		, /col  $
		, YPAD = 3 $
		, TITLE = 'InputPanel')

	LeafParameterPanel = Widget_Base(MainProspectPanel, UNAME = 'LeafParameterPanel'  $
		, /Frame, /col $
		, XPAD = 5)

	LeafParameterTitle = Widget_Label(LeafParameterPanel, UNAME = 'LeafParameterTitle'  $
		, /ALIGN_LEFT  $
		, VALUE='Leaf input parameters')

	; Leaf structure parameter UI
	LeafStructurePanel = Widget_base(LeafParameterPanel, UNAME = 'LeafStructurePanel', /row)

	LeafStructureLabel = Widget_label(LeafStructurePanel, UNAME = 'LeafStructureLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'N')

	LeafStructureSlider = CW_inform_FSlider(LeafStructurePanel, TITLE = '' $
		, VALUE = 2 $
		, MINIMUM = 1 $
		, MAXIMUM = 3 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'LeafStructureSlider' ,/EDIT)

	LeafStructureDescLabel = Widget_label(LeafStructurePanel, UNAME = 'LeafStructureDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Leaf structure parameter, default = 2')


	; Chlorophyll parameter UI
	ChloroContentPanel = Widget_base(LeafParameterPanel, UNAME = 'ChloroContentPanel', /row)

	ChloroContentLabel = Widget_label(ChloroContentPanel, UNAME = 'ChloroContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'Cab')

	ChloroContentSlider = CW_inform_FSlider(ChloroContentPanel, TITLE = '' $
		, VALUE = 60.0 $
		, MINIMUM = 0.0 $
		, MAXIMUM = 100.0 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'ChloroContentSlider' ,/EDIT)

	ChloroContentDescLabel = Widget_label(ChloroContentPanel, UNAME = 'ChloroContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Chlorophyll a+b content in �g/cm�, default = 60')


	; Equivalent water thickness parameter UI
	EquivWaterPanel = Widget_base(LeafParameterPanel, UNAME = 'EquivWaterPanel', /row)

	EquivWaterLabel = Widget_label(EquivWaterPanel, UNAME = 'EquivWaterLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'Cw')

	EquivWaterSlider = CW_inform_FSlider(EquivWaterPanel, TITLE = '' $
		, VALUE = 0.025 $
		, MINIMUM = 0.0 $
		, MAXIMUM = 0.05 $
		, SCROLL = 0.001 $
		, FORMAT = '(F0.3)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'EquivWaterSlider' ,/EDIT)

	EquivWaterDescLabel = Widget_label(EquivWaterPanel, UNAME = 'EquivWaterDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Equivalent water thickness in g/cm�, default = 0.025')

	; Dry matter content parameter UI
	DryMatterContentPanel = Widget_base(LeafParameterPanel, UNAME = 'DryMatterContentPanel', /row)

	DryMatterContentLabel = Widget_label(DryMatterContentPanel, UNAME = 'DryMatterContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'Cm')

	DryMatterContentSlider = CW_inform_FSlider(DryMatterContentPanel, TITLE = '' $
		, VALUE = 0.025 $
		, MINIMUM = 0.002 $
		, MAXIMUM = 0.04 $
		, SCROLL = 0.001 $
		, FORMAT = '(F0.3)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'DryMatterContentSlider' ,/EDIT)

	DryMatterContentDescLabel = Widget_label(DryMatterContentPanel, UNAME = 'DryMatterContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Dry matter content in g/cm�, default = 0.025')

	; Canopy
	CanopyParameterPanel = Widget_Base(MainProspectPanel, UNAME = 'CanopyParameterPanel'  $
		, /Frame, /col $
		, XPAD = 5)

	CanopyParameterTitle = Widget_Label(CanopyParameterPanel, UNAME = 'CanopyParameterTitle'  $
		, /ALIGN_LEFT  $
		, VALUE='Canopy input parameters')


	; 'hot' parameter UI
	HotContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'HotContentPanel', /row)

	HotContentLabel = Widget_label(HotContentPanel, UNAME = 'HotContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'Hot')

	HotContentSlider = CW_inform_FSlider(HotContentPanel, TITLE = '' $
		, VALUE = 0.02 $
		, MINIMUM = 0.01 $
		, MAXIMUM = 0.05 $
		, SCROLL = 0.001 $
		, FORMAT = '(F0.3)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'HotContentSlider' ,/EDIT)

	HotContentDescLabel = Widget_label(HotContentPanel, UNAME = 'HotContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Hot spot parameter, default = 0.02')

	; Scale factor parameter UI
	ScaleFactorContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'ScaleFactorContentPanel', /row)

	ScaleFactorContentLabel = Widget_label(ScaleFactorContentPanel, UNAME = 'ScaleFactorContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'Scale')

	ScaleFactorContentSlider = CW_inform_FSlider(ScaleFactorContentPanel, TITLE = '' $
		, VALUE = 1 $
		, MINIMUM = 0 $
		, MAXIMUM = 1 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'ScaleFactorContentSlider' ,/EDIT)

	ScaleFactorContentDescLabel = Widget_label(ScaleFactorContentPanel, UNAME = 'ScaleFactorContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Scale factor, default = 1')

	; LAI parameter UI
	LeafAreaIndexContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'LeafAreaIndexContentPanel', /row)

	LeafAreaIndexContentLabel = Widget_label(LeafAreaIndexContentPanel, UNAME = 'LeafAreaIndexContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'LAIs')

	LeafAreaIndexContentSlider = CW_inform_FSlider(LeafAreaIndexContentPanel, TITLE = '' $
		, VALUE = 7 $
		, MINIMUM = 0 $
		, MAXIMUM = 10 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'LeafAreaIndexContentSlider' ,/EDIT)

	LeafAreaIndexContentDescLabel = Widget_label(LeafAreaIndexContentPanel, UNAME = 'LeafAreaIndexContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Single tree leaf area index in m�/m�, default = 7')

	; LAI for understorey parameter UI
	LAIUnderContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'LAIUnderContentPanel', /row)

	LAIUnderContentLabel = Widget_label(LAIUnderContentPanel, UNAME = 'LAIUnderContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'LAIU')

	LAIUnderContentSlider = CW_inform_FSlider(LAIUnderContentPanel, TITLE = '' $
		, VALUE = 0.1 $
		, MINIMUM = 0.0 $
		, MAXIMUM = 3.0 $
		, SCROLL = 0.01 $
		, FORMAT = '(F0.2)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'LAIUnderContentSlider' ,/EDIT)

	LAIUnderContentDescLabel = Widget_label(LAIUnderContentPanel, UNAME = 'LAIUnderContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Leaf area index of understorey in m�/m�, default = 0.1')

	; Stem Density parameter UI
	StemDensityContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'StemDensityContentPanel', /row)

	StemDensityContentLabel = Widget_label(StemDensityContentPanel, UNAME = 'StemDensityContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'sd')

	StemDensityContentSlider = CW_inform_FSlider(StemDensityContentPanel, TITLE = '' $
		, VALUE = 650 $
		, MINIMUM = 0 $
		, MAXIMUM = 4000 $
		, SCROLL = 25 $
		, FORMAT = '(I0)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'StemDensityContentSlider' ,/EDIT)

	StemDensityContentDescLabel = Widget_label(StemDensityContentPanel, UNAME = 'StemDensityContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Stem density in stems/ha, default = 650')

	; Tree height parameter UI
	TreeHeightContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'TreeHeightContentPanel', /row)

	TreeHeightContentLabel = Widget_label(TreeHeightContentPanel, UNAME = 'TreeHeightContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'h')

	TreeHeightContentSlider = CW_inform_FSlider(TreeHeightContentPanel, TITLE = '' $
		, VALUE = 20 $
		, MINIMUM = 0 $
		, MAXIMUM = 50 $
		, SCROLL = 1 $
		, FORMAT = '(I0)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'TreeHeightContentSlider' ,/EDIT)

	TreeHeightContentDescLabel = Widget_label(TreeHeightContentPanel, UNAME = 'TreeHeightContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Tree height in m, default = 20')

	; Crown diameter parameter UI
	CrownDiameterContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'CrownDiameterContentPanel', /row)

	CrownDiameterContentLabel = Widget_label(CrownDiameterContentPanel, UNAME = 'CrownDiameterContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'cd')

	CrownDiameterContentSlider = CW_inform_FSlider(CrownDiameterContentPanel, TITLE = '' $
		, VALUE = 4.5 $
		, MINIMUM = 0 $
		, MAXIMUM = 10 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'CrownDiameterContentSlider' ,/EDIT)

	CrownDiameterContentDescLabel = Widget_label(CrownDiameterContentPanel, UNAME = 'CrownDiameterContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Crown diameter in m, default = 4.5')

	; Average leaf angle of canopy UI
	AvgCanopyLeafAngleContentPanel = Widget_base(CanopyParameterPanel, UNAME = 'AvgCanopyLeafAngleContentPanel', /row)

	AvgCanopyLeafAngleContentLabel = Widget_label(AvgCanopyLeafAngleContentPanel, UNAME = 'AvgCanopyLeafAngleContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'ala')

	AvgCanopyLeafAngleContentSlider = CW_inform_FSlider(AvgCanopyLeafAngleContentPanel, TITLE = '' $
		, VALUE = 55 $
		, MINIMUM = 0 $
		, MAXIMUM = 89 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'AvgCanopyLeafAngleContentSlider' ,/EDIT)

	AvgCanopyLeafAngleContentDescLabel = Widget_label(AvgCanopyLeafAngleContentPanel, UNAME = 'AvgCanopyLeafAngleContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Average leaf angle of canopy in degree, default = 55')

	; External inputs
	ExternalParameterPanel = Widget_Base(MainProspectPanel, UNAME = 'ExternalParameterPanel'  $
		, /Frame, /col $
		, XPAD = 5)

	ExternalParameterTitle = Widget_Label(ExternalParameterPanel, UNAME = 'ExternalParameterTitle'  $
		, /ALIGN_LEFT  $
		, VALUE='External input parameters')

	; Observation zenith angle parameter UI
	SunZenithAngleContentPanel = Widget_base(ExternalParameterPanel, UNAME = 'SunZenithAngleContentPanel', /row)

	SunZenithAngleContentLabel = Widget_label(SunZenithAngleContentPanel, UNAME = 'SunZenithAngleContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'theta_s')

	SunZenithAngleContentSlider = CW_inform_FSlider(SunZenithAngleContentPanel, TITLE = '' $
		, VALUE = 30 $
		, MINIMUM = 0 $
		, MAXIMUM = 89 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'SunZenithAngleContentSlider' ,/EDIT)

	SunZenithAngleContentDescLabel = Widget_label(SunZenithAngleContentPanel, UNAME = 'SunZenithAngleContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Sun zenith angle in degree, default = 30')

	; Observation zenith angle parameter UI
	ObservationZenithAngleContentPanel = Widget_base(ExternalParameterPanel, UNAME = 'ObservationZenithAngleContentPanel', /row)

	ObservationZenithAngleContentLabel = Widget_label(ObservationZenithAngleContentPanel, UNAME = 'ObservationZenithAngleContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'theta_o')

	ObservationZenithAngleContentSlider = CW_inform_FSlider(ObservationZenithAngleContentPanel, TITLE = '' $
		, VALUE = 0 $
		, MINIMUM = 0 $
		, MAXIMUM = 89 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'ObservationZenithAngleContentSlider' ,/EDIT)

	ObservationZenithAngleContentDescLabel = Widget_label(ObservationZenithAngleContentPanel, UNAME = 'ObservationZenithAngleContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Observation zenith angle in degree, default = 0')

	; Azimuth angle parameter UI
	AzimuthAngleContentPanel = Widget_base(ExternalParameterPanel, UNAME = 'AzimuthAngleContentPanel', /row)

	AzimuthAngleContentLabel = Widget_label(AzimuthAngleContentPanel, UNAME = 'AzimuthAngleContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'phi')

	AzimuthAngleContentSlider = CW_inform_FSlider(AzimuthAngleContentPanel, TITLE = '' $
		, VALUE = 0 $
		, MINIMUM = 0 $
		, MAXIMUM = 180 $
		, SCROLL = 0.1 $
		, FORMAT = '(F0.1)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'AzimuthAngleContentSlider' ,/EDIT)

	AzimuthAngleContentDescLabel = Widget_label(AzimuthAngleContentPanel, UNAME = 'AzimuthAngleContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Azimuth angle in degree, default = 0')

	; Fraction of diffuse radiation parameter UI
	DiffuseRadiationContentPanel = Widget_base(ExternalParameterPanel, UNAME = 'DiffuseRadiationContentPanel', /row)

	DiffuseRadiationContentLabel = Widget_label(DiffuseRadiationContentPanel, UNAME = 'DiffuseRadiationContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'skyl')

	DiffuseRadiationContentSlider = CW_inform_FSlider(DiffuseRadiationContentPanel, TITLE = '' $
		, VALUE = 0.1 $
		, MINIMUM = 0.0 $
		, MAXIMUM = 1.0 $
		, SCROLL = 0.01 $
		, FORMAT = '(F0.2)' $
		, EVENT_PRO = 'updateDisplays' $
		, UVALUE = 'DiffuseRadiationContentSlider' ,/EDIT)

	DiffuseRadiationContentDescLabel = Widget_label(DiffuseRadiationContentPanel, UNAME = 'DiffuseRadiationContentDescLabel' $
		, /ALIGN_LEFT $
		, Value = 'Fraction of diffuse radiation, default = 0.1')

	; Other inputs
	OtherParameterPanel = Widget_Base(MainProspectPanel, UNAME = 'OtherParameterPanel'  $
		, /Frame, /col $
		, XPAD = 5)

	OtherParameterTitle = Widget_Label(OtherParameterPanel, UNAME = 'OtherParameterTitle'  $
		, /ALIGN_LEFT  $
		, VALUE='File input parameters')

	; soil spectrum parameter UI
	SoilSpectrumDefaultPanel = widget_base(OtherParameterPanel, UNAME = 'SoilSpectrumDefaultPanel', /col, /NONEXCLUSIVE)

	SoilSpectrumDefaultButton = widget_button(SoilSpectrumDefaultPanel, uname = 'SoilSpectrumDefaultButton', /align_left, value = 'Use default soil spectrum')

	SoilSpectrumContentPanel = Widget_base(OtherParameterPanel, UNAME = 'SoilSpectrumContentPanel', /row)

	SoilSpectrumContentLabel = Widget_label(SoilSpectrumContentPanel, UNAME = 'SoilSpectrumContentLabel' $
		, /ALIGN_LEFT, XSIZE = parLabelSize $
		, Value = 'r_soil')

	SoilSpectrumContentText = Widget_Text(SoilSpectrumContentPanel, UNAME = 'SoilSpectrumContentText' $
		,/EDITABLE ,/ALL_EVENTS ,XSIZE = 60 ,YSIZE = 1)

	SoilSpectrumContentBrowseButton = Widget_Button(SoilSpectrumContentPanel, UNAME = 'SoilSpectrumContentBrowseButton' $
		,/ALIGN_CENTER $
		,TOOLTIP='Browse for soil spectrum' ,VALUE='...')
	statevalue.soilspectrum_input = SoilSpectrumContentText

	; Add the UI text fields in the statevalue, to pass them to event handlers
	statevalue.leafStructure		= LeafStructureSlider
	statevalue.leafChlorophyl 		= ChloroContentSlider
	statevalue.leafEqWater 			= EquivWaterSlider
	statevalue.leafDryMatter 		= DryMatterContentSlider
	statevalue.canopyHot 			= HotContentSlider
	statevalue.canopyScale 			= ScaleFactorContentSlider
	statevalue.canopyLAI 			= LeafAreaIndexContentSlider
	statevalue.canopyLAIU 			= LAIUnderContentSlider
	statevalue.canopyStemDensity 	= StemDensityContentSlider
	statevalue.canopyTreeHeight 	= TreeHeightContentSlider
	statevalue.canopyCrownDiam 		= CrownDiameterContentSlider
	statevalue.canopyAvgLeafAngle 	= AvgCanopyLeafAngleContentSlider
	statevalue.externalTetaS 		= SunZenithAngleContentSlider
	statevalue.externalTetaO 		= ObservationZenithAngleContentSlider
	statevalue.externalPhi 			= AzimuthAngleContentSlider
	statevalue.externalSkyl 		= DiffuseRadiationContentSlider

	statevalue.SoilSpectrumPanel	= SoilSpectrumContentPanel
	statevalue.UseDefaultSoil		= SoilSpectrumDefaultButton

	; initialize the defaults
	resetToDefaults, statevalue

	;	set soil spectrum panel to defined state
	widget_control, SoilSpectrumDefaultButton, /SET_BUTTON
	widget_control, SoilSpectrumContentPanel, sensitive = 0
end

; create the page with the wavelength / reflectance graphs
pro inform_createWavelengthPage, parent, statevalue, color_draws

	DrawProspectPanel = Widget_Base(parent, UNAME = 'DrawProspectPanel'  $
		, /col  $
		, YPAD = 3 $
		, TITLE = 'Wavelength')

	; The graph pane for the visible output
	View = widget_draw(DrawProspectPanel $
		, X_SCROLL_SIZE = 500 $
		, Y_SCROLL_SIZE = 375 $
		, XSIZE = 500 $
		, YSIZE = 375 $
		, /APP_SCROLL $
		, RETAIN = 2 $
		)

	OutputPanel = widget_base(DrawProspectPanel, uname = 'OutputPanel', /col, /frame)
	outputTitleLabel = Widget_label(OutputPanel, UNAME = 'outputTitleLabel' $
		, /ALIGN_LEFT $
		, Value = 'Select output for display')

	; UI for selection of display outputs
	SelectOutputPanel = widget_base(OutputPanel, UNAME = 'SelectOutputPanel', /row)
	SelectOutputColorLeft = widget_base(SelectOutputPanel, UNAME = 'SelectOutputColorLeft', /col, YPAD = 2, space = 5)
	SelectOutputPanelLeft = widget_base(SelectOutputPanel, UNAME = 'SelectOutputPanelLeft', /col, /NONEXCLUSIVE)
	SelectOutputColorRight = widget_base(SelectOutputPanel, UNAME = 'SelectOutputColorRight', /col, YPAD = 2, space = 5)
	SelectOutputPanelRight = widget_base(SelectOutputPanel, UNAME = 'SelectOutputPanelRight', /col, /NONEXCLUSIVE)

	color_draws.drawForestColor = widget_draw(SelectOutputColorLeft, XSize = 15, YSize = 15, /frame)
	ForestViewOutput = widget_button(SelectOutputPanelLeft, uname = 'ForestViewOutput', /align_left, value = 'Forest reflectance')

	color_draws.drawSoilColor = widget_draw(SelectOutputColorLeft, XSize = 15, YSize = 15, /frame)
	SoilViewOutput = widget_button(SelectOutputPanelLeft, uname = 'SoilViewOutput', /align_left, value = 'Soil reflectance')

	color_draws.drawUnderstoreyColor = widget_draw(SelectOutputColorLeft, XSize = 15, YSize = 15, /frame)
	UnderViewOutput = widget_button(SelectOutputPanelLeft, uname = 'UnderViewOutput', /align_left, value = 'Understorey reflectance')

	color_draws.drawInfCanopyColor = widget_draw(SelectOutputColorLeft, XSize = 15, YSize = 15, /frame)
	InfCanopyViewOutput = widget_button(SelectOutputPanelLeft, uname = 'InfCanopyViewOutput', /align_left, value = 'Infinite canopy reflectance')

	color_draws.drawLeafRefColor = widget_draw(SelectOutputColorLeft, XSize = 15, YSize = 15, /frame)
	LeafViewOutput = widget_button(SelectOutputPanelLeft, uname = 'LeafViewOutput', /align_left, value = 'Leaf reflectance')

	color_draws.drawLeafTransColor = widget_draw(SelectOutputColorRight, XSize = 15, YSize = 15, /frame)
	LeafTransViewOutput = widget_button(SelectOutputPanelRight, uname = 'LeafTransViewOutput', /align_left, value = 'Leaf transmittance')

	color_draws.drawCrownSColor = widget_draw(SelectOutputColorRight, XSize = 15, YSize = 15, /frame)
	CrownTransTeta_s = widget_button(SelectOutputPanelRight, uname = 'CrownTransTeta_s', /align_left, value = 'Crown transmittance for teta_s')

	color_draws.drawCrownOColor = widget_draw(SelectOutputColorRight, XSize = 15, YSize = 15, /frame)
	CrownTransTeta_o = widget_button(SelectOutputPanelRight, uname = 'CrownTransTeta_o', /align_left, value = 'Crown transmittance for teta_o')

	color_draws.drawGroundColor = widget_draw(SelectOutputColorRight, XSize = 15, YSize = 15, /frame)
	GroundFactorOutput = widget_button(SelectOutputPanelRight, uname = 'GroundFactorOutput', /align_left, value = 'Ground factor')

	TextualOutputPanel = widget_base(OutputPanel, UNAME = 'TextualOutputPanel', /row)

	crownCoverPanel = widget_base(TextualOutputPanel, uname = 'crownCoverPanel', /row)
	crownCoverLabel = widget_label(crownCoverPanel, value = 'Crown cover')
	crownCoverText  = widget_text(crownCoverPanel, value = '')
	LAIcPanel = widget_base(TextualOutputPanel, uname = 'LAIcPanel', /row)
	LAIcLabel = widget_label(LAIcPanel, value = 'LAIc')
	LAIcText  = widget_text(LAIcPanel, value = '')

	inform_MeasuredSpectrumPanel = widget_base(DrawProspectPanel, /frame, /col)
	inform_CheckPanel = widget_base(inform_MeasuredSpectrumPanel, /row)
	inform_CheckSubPanel = widget_base(inform_CheckPanel)
	color_draws.inform_drawMeasureColor = widget_draw(inform_CheckSubPanel, XSize = 15, YSize = 15, yoffset = 5, /frame)
	inform_MSpectrumCheckPanel = widget_base(inform_CheckPanel, /row, /nonexclusive)
	inform_MSpectrumCheckButton = widget_button(inform_MSpectrumCheckPanel, value = 'Show measured spectrum', uname= 'inform_MSpectrumCheckButton')
	inform_MSpectrumLoadPanel = widget_base(inform_MeasuredSpectrumPanel, /row)
	inform_MeasuredSpectrumLabel = widget_label(inform_MSpectrumLoadPanel, value = 'Measured spectrum')
	inform_MeasuredSpectrumText = widget_text(inform_MSpectrumLoadPanel, value = '' $
				, /editable $
				, XSIZE = 59 $
			)
	inform_MeasuredSpectrumBrowse = widget_button(inform_MSpectrumLoadPanel, uname='inform_MeasuredSpectrumBrowse' $
				, event_pro = 'inform_handleMeasuredSpectrumBrowse' $
				, value = '...' $
			)

	statevalue.draw_pane 			= View
	statevalue.plot_r_forest		= ForestViewOutput
	statevalue.plot_r_soil			= SoilViewOutput
	statevalue.plot_r_under			= UnderViewOutput
	statevalue.plot_r_c_inf			= InfCanopyViewOutput
	statevalue.plot_r_leaf			= LeafViewOutput
	statevalue.plot_t_leaf			= LeafTransViewOutput
	statevalue.plot_t_s				= CrownTransTeta_s
	statevalue.plot_t_o				= CrownTransTeta_o
	statevalue.plot_G				= GroundFactorOutput

	statevalue.print_CO				= crownCoverText
	statevalue.print_LAIc			= LAIcText

	statevalue.measuredSpectrum		= inform_MeasuredSpectrumText
	statevalue.measuredPanel		= inform_MSpectrumLoadPanel
	statevalue.measureSpectrumChk	= inform_MSpectrumCheckButton

	;	set measured spectrum panel to defined state
	widget_control, inform_MSpectrumLoadPanel, sensitive = 0

end

; observer angle against reflectance (multiple runs of the model)
; The graph pane for the reflectance as function of the view angle
pro inform_createAnglePage, parent, statevalue

	DrawAnglePanel = Widget_Base(parent, UNAME = 'DrawAnglePanel'  $
		, /col  $
		, YPAD = 3 $
		, TITLE = 'Angle')


	ViewAngles = widget_draw(DrawAnglePanel $
		, X_SCROLL_SIZE = 500 $
		, Y_SCROLL_SIZE = 375 $
		, XSIZE = 500 $
		, YSIZE = 375 $
		, /APP_SCROLL $
		, RETAIN = 2 $
		)

	selectPanel = widget_base(DrawAnglePanel, /row, /frame)
	angleSelectPanel = widget_base(selectPanel, /col)
	reflectSelectPanel = widget_base(selectPanel, /row)

	reflectLabel = widget_label(reflectSelectPanel, value = 'Reflectance' $
			)
	reflectSelectComboBox = widget_combobox(reflectSelectPanel, uname = 'reflectSelectComboBox' $
;			, tooltip = 'Select the reflectance values to display in the plot' $
			, value = ['Forest', 'Understorey', 'Infinite canopy', 'Leaf' ] $
			)

	wavelengthPanel = widget_base(angleSelectPanel, /col)
	wavelengthLabel = widget_label(wavelengthPanel, value = 'Wavelengths')
	wavelengthSelectList = widget_list(wavelengthPanel, uname = 'wavelengthSelectList' $
			, / multiple  $
			, ysize = 5 $
			,VALUE = [  '400',  '500',  '600',  '700',  '800'  $
					 ,  '900', '1000', '1100', '1200', '1300'  $
					 , '1400', '1500', '1600', '1700', '1800'  $
					 , '1900', '2000', '2100', '2200', '2300', '2400', '2500']  $
		)

	angleUpdatePanel = widget_base(angleSelectPanel, /row)
	viewSelectCombobox = widget_combobox(angleUpdatePanel, uname = 'viewSelectCombobox' $
			,VALUE = [ 'Observation zenith angle', 'Sun zenith angle' ] $
		)

	statevalue.draw_anglePane		= ViewAngles
	statevalue.select_AngleCB		= viewSelectCombobox
	statevalue.wavelengthSelectList = wavelengthSelectList
	statevalue.reflectSelectComboBox = reflectSelectComboBox

end

pro	inform_createSensitivityPage, SensitivityPanel, statevalue
	label_width = 100

	sensOutputPanel = widget_base(SensitivityPanel, uname= 'sensOutputPanel' $
			, TITLE = 'Sensitivity Analysis' $
			, /col $
		)

	SensitivityInputPanel = Widget_Base(sensOutputPanel, UNAME = 'SensitivityInputPanel'  $
		, col = 1  $
		, YPAD = 3 $
		, /frame   $
		, TITLE = 'Sensitivity')

	sensParameterPanel = widget_base(SensitivityInputPanel, uname= 'sensParameterPanel', /row)

	sensParameterLabel = widget_label(sensParameterPanel $
			,/align_left, xsize = label_width $
			,value = 'Input parameter' $
		)

	sensParameterCombobox = widget_combobox(sensParameterPanel  $
			, uname = 'sensParameterCombobox' $
			, value = getParameterField(-1,0) $
		)

	sensParameterRangePanel = widget_base(SensitivityInputPanel, uname= 'sensParameterRangePanel', /col)

	sensParameterRangeLabel = widget_label(sensParameterRangePanel $
			,/align_left, xsize = label_width $
		 	,value = 'Range' $
		 )

	; Range: minimum field UI
	sensParameterRangeMinPanel = widget_base(sensParameterRangePanel, /row)
	sensParameterRangeMinLabel = widget_label(sensParameterRangeMinPanel $
			,/align_left, xsize = label_width $
			, value = 'Minimum' $
		)
	sensParameterRangeMinText = widget_text(sensParameterRangeMinPanel $
			, value = string(getParameterField(0, 3)) $
			, uname = 'sensParameterRangeMinText' $
		)

	; Range: maximum field UI
	sensParameterRangeMaxPanel = widget_base(sensParameterRangePanel, /row)
	sensParameterRangeMaxLabel = widget_label(sensParameterRangeMaxPanel $
			,/align_left, xsize = label_width $
			, value = 'Maximum' $
		)
	sensParameterRangeMaxText = widget_text(sensParameterRangeMaxPanel $
			, value = string(getParameterField(0, 4)) $
			, uname = 'sensParameterRangeMaxText' $
		)

	; Range: steps field UI
	sensParameterRangeStepsPanel = widget_base(sensParameterRangePanel, /row)
	sensParameterRangeStepsLabel = widget_label(sensParameterRangeStepsPanel $
			,/align_left, xsize = label_width $
			, value = 'Steps' $
		)
	sensParameterRangeStepsText = widget_text(sensParameterRangeStepsPanel $
			, uname = 'sensParameterRangeStepsText' $
			, /sensitive, /editable $
			, /all_events $
			, value = '10' $
		)

	; Now the output draw view
	; The graph pane for the visible sensitivity output
	sensView = widget_draw(sensOutputPanel $
		, uname = 'sensView' $
		, X_SCROLL_SIZE = 500 $
		, Y_SCROLL_SIZE = 375 $
		, XSIZE = 500 $
		, YSIZE = 375 $
		, /APP_SCROLL $
		, RETAIN = 2 $
		)

	sensSelectPanel = widget_base(sensOutputPanel, /row, /frame)
	sensReflectSelectPanel = widget_base(sensSelectPanel, /row)

	reflectLabel = widget_label(sensReflectSelectPanel, value = 'Reflectance' $
			)
	sensReflectSelectComboBox = widget_combobox(sensReflectSelectPanel, uname = 'sensReflectSelectComboBox' $
			, value = ['Forest', 'Understorey', 'Infinite canopy', 'Leaf' ] $
		)
end

pro	inform_createInverseSensitivityPage, parent_panel, statevalue
	label_width = 100

	invSensOutputPanel = widget_base(parent_panel, uname= 'invSensOutputPanel' $
			, TITLE = 'Inverse Mode' $
			, /col $
		)

	inputsPanel = widget_base(invSensOutputPanel, /col, /frame)
	stepsParamPanel = widget_base(inputsPanel, /row)
	steps_label = widget_label(stepsParamPanel, value = 'Number of steps per parameter range:')
	steps_text = widget_text(stepsParamPanel, value = string(10, format = '(i0)') $
		, uname = 'steps_text' $
		, /sensitiv, /editable, xsize = 4 $
	)

	sensReflectSelectPanel = widget_base(inputsPanel, /row)
	reflectLabel = widget_label(sensReflectSelectPanel, value = 'Reflectance', xsize = label_width)
	invSensReflectSelectComboBox = widget_combobox(sensReflectSelectPanel, uname = 'invSensReflectSelectComboBox' $
			, value = ['Forest', 'Understorey', 'Infinite canopy', 'Leaf' ] $
		)

	refSpectrumPanel = widget_base(inputsPanel, /row)
	refSpectrumLabel = widget_label(refSpectrumPanel, value = 'Reference spectrum:', xsize = label_width)
	refSpectrumText = widget_text(refSpectrumPanel, value = '', /sens, /edit, xsize = 59, uname = 'refSpectrumText')
	refSpectrumBrowseButton = widget_button(refSpectrumPanel, value = '...', uname = 'refSpectrumBrowseButton')

	InvSensitivityInputPanel = Widget_Base(invSensOutputPanel, UNAME = 'InvSensitivityInputPanel'  $
		, col = 2  $
		, YPAD = 3 $
		, TITLE = 'Reverse Mode')

	invSensParameterPanel1 = widget_base(InvSensitivityInputPanel, uname= 'invSensParameterPanel1', /frame, /col)
	invSensParameterPanel2 = widget_base(InvSensitivityInputPanel, uname= 'invSensParameterPanel2', /frame, /col)

	dummy = getParameterField(0, -1)
	par_array = replicate(dummy, getParameterCount())
	for p = 0, getParameterCount() / 2 - 1 do begin
		inform_add_check_minmax, invSensParameterPanel1, p, par_val
		par_array[p] = par_val
	endfor
	for p = getParameterCount() / 2, getParameterCount() - 1 do begin
		inform_add_check_minmax, invSensParameterPanel2, p, par_val
		par_array[p] = par_val
	endfor

	runButtonPanel = widget_base(invSensOutputPanel, /row, /align_right)
	invSensRunButton = widget_button(runButtonPanel, value = 'Start Analysis', uname = 'invSensRunButton')

	fld = widget_text(invSensOutputPanel, ysize = 16 + 2, xsize = 60 $
		, /scroll $
		, font='Courier*8', /sensitive $
		, uname = 'InverseModeResultText' $
	)
end

function valtostr, val, par_val
	return, string(val, format = par_val.format)
end

pro inform_add_check_minmax, parent_panel, index, par_val
	par_val = getParameterField(index, -1)
	par_panel = widget_base(parent_panel, /row)
	chkbox_panel = widget_base(par_panel, /nonexclusive)
	uname = 'chkbox_par_' + par_val.desg
	chkbox = widget_button(chkbox_panel, uname = uname, value = par_val.desg, xsize = 60)

	min_max_panel = widget_base(par_panel, /row)
	min_label = widget_label(min_max_panel, value = 'min')
	uname = 'text_par_' + par_val.desg + '_min'

	sval = valtostr(par_val.min_val, par_val)
	min_text = widget_text(min_max_panel, value = sval, uname = uname $
		, xsize = 8 $
		, /sensitiv, /editable $
	)

	max_label = widget_label(min_max_panel, value = 'max')
	uname = 'text_par_' + par_val.desg + '_max'
	sval = valtostr(par_val.max_val, par_val)
	max_text = widget_text(min_max_panel, value = sval, uname = uname $
		, xsize = 8 $
		, /sensitiv, /editable $
	)
end

pro init_draw_model_tab, statevalue, color_draws
	; initialize the wavelength draw pane
  	Widget_Control, statevalue.draw_pane, Get_Value = dispID
	wset, dispID
    DEVICE, DECOMPOSED = 0
	loadct,12, /silent	; 16 level color lut
	freq = indgen(421) * 5 + 400
	; display the axes
	plot, freq $
		, BACKGROUND = 255, COLOR = 0 $
		, /nodata $
		, xrange = [400, 2500], /xstyle $
		, xtickinterval = 400 $
		, yrange = [0.0, 1.0], /ystyle $
      	, Position = [0.15, 0.15, 0.85, 0.85] $
		, title = 'Spectral Reflectance Variation' $
      	, font = 0 $
      	, XTitle = 'Wavelength [nm]' $
      	, YTitle = 'Reflectance'

	drawColor, color_draws.drawForestColor, 24
	drawColor, color_draws.drawSoilColor, 232
	drawColor, color_draws.drawUnderstoreyColor, 8
	drawColor, color_draws.drawInfCanopyColor, 104
	drawColor, color_draws.drawLeafRefColor, 40
	drawColor, color_draws.drawLeafTransColor, 88
	drawColor, color_draws.drawCrownSColor, 120
	drawColor, color_draws.drawCrownOColor, 136
	drawColor, color_draws.drawGroundColor, 216
	drawColor, color_draws.inform_drawMeasureColor, 200

	; initialize the angles draw pane
	angles = indgen(180) - 90
	zero_n = fltarr(n_elements(angles))
	inform_drawAngle, statevalue.draw_anglePane, angles, zero_n, 'Observation zenith angle', 12

end

pro init_draw_sens_tab, top, freq = freq
	; initialize the wavelength draw pane
	draw_pane = Widget_Info(top, find_by_uname='sensView')
  	Widget_Control, draw_pane, Get_Value = dispID
	wset, dispID
    DEVICE, DECOMPOSED = 0
	loadct,12, /silent	; 16 level color lut
	if n_elements(freq) eq 0 then $
		freq = indgen(421) * 5 + 400
	; display the axes
	plot, freq $
		, BACKGROUND = 255, COLOR = 0 $
		, /nodata $
		, xrange = [400, 2500], /xstyle $
		, xtickinterval = 400 $
		, yrange = [0.0, 1.0], /ystyle $
      	, Position = [0.15, 0.15, 0.85, 0.85] $
		, title = 'Spectral Reflectance Variation' $
      	, font = 0 $
      	, XTitle = 'Wavelength [nm]' $
      	, YTitle = 'Reflectance'
end

pro drawColor, drawer, color
  	Widget_Control, drawer, Get_Value = dispID
  	WSet, dispID
;  	DEVICE, DECOMPOSED = 0
	loadct, 12, /silent	; 16 level color lut
  	Erase, Color = color
end

pro nrs_prospect_extensions_init
  compile_opt IDL2

  e = ENVI(/CURRENT)
  e.AddExtension, 'INFORM Prospect', 'nrs_prospect_gui', PATH='NRS'
end

; For ENVI 5 compatibility
pro nrs_prospect
  compile_opt IDL2
  
  prospect_gui
end



