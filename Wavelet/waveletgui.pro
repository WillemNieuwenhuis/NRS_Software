pro waveletgui_extensions_init
  compile_opt IDL2
  
  e = ENVI(/CURRENT)
  e.AddExtension, 'Wavelet processing', 'waveletgui', PATH='NRS'
end

pro waveletgui
  compile_opt IDL2
  
  catch, err
  if (err ne 0) then begin
    catch, /cancel
    if obj_valid(e) then $
      e.reporterror, 'Error: ' + !error_state.msg
    message, /reset
    return
  endif
  
  e = envi(/current)
  
  wavelet, 0
end

pro WaveletGUI_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

;-----------
; Wavelet page events
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseInputButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseInput, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseCrystalFileButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleCrystalFile, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='InverseCrystalFileButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleInverseCrystalFile, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseLoadMultiresButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseLoadCoefficient, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseMultiresButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseCoefficient, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CalcButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleCalculate, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='StatisticsButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleStatistics, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='SurfaceButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleCoefficientSurface, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='AllLevelsToggle'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleToggleAllLevels, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='ExtractCrystalButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleCrystalExtract, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='InvTransformButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleCrystalReverseTransform, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='CrystalComboBox'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        handleCrystalChange, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='InverseCrystalComboBox'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_COMBOBOX' )then $
        handleInverseCrystalChange, Event
    end
    widget_info(wWidget, FIND_BY_UNAME='InverseThresholdButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleThresholdCount, Event
    end
;-----------
; Batch page events
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseInputListButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBatchBrowseInput, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseBatchMultiresButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBatchBrowseCoefficient, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CalcBatchButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBatchCalculate, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='inputSplitToggle'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleSplitToggle, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='BrowseSplitImageListButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleBrowseSplitImage, Event
    end
;-----------
; Fusion page events
    Widget_Info(wWidget, FIND_BY_UNAME='FusionCalcButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        wav_handleFusionCalculate, Event
    end
    
;-----------
; button panel events
    Widget_Info(wWidget, FIND_BY_UNAME='CloseButton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        handleClose, Event
    end
    else:
  endcase

end

pro waveletgui_create_batchPage, wvbatch_form, text_size
; Create the contents of the batch page
;-------------
  label_size = 80

  ; Selection of the input image data
  inputOptionsPanel = widget_base(wvbatch_form, /col, /frame)
  
  ; input split option
  inputSplitCheckBoxBase = widget_base(inputOptionsPanel, title='Split large file select', /col, /nonexclusive)
  inputSplitToggle = widget_button(inputSplitCheckBoxBase, uname='inputSplitToggle'  $
    ,/align_left ,value='Split large image')
  
  ; input split option details
  inputSplitImageNamePanel = widget_base(inputOptionsPanel, /row, uname = 'inputSplitImageNamePanel',  sensitive = 0)
  InputSplitImageNameLabel = widget_label(inputSplitImageNamePanel, uname='InputSplitImageNameLabel'  $
    ,scr_xsize = label_size $
    ,/align_left ,value='Input image:')

  InputSplitImageNameText = widget_text(inputSplitImageNamePanel, uname='InputSplitImageNameText' $
    , scr_xsize=text_size + 35   $
    ,/editable ,/all_events)

  BrowseSplitImageListButton = widget_button(inputSplitImageNamePanel, uname='BrowseSplitImageListButton' $
    ,/align_center ,value='...')

  ; input split option size of new images
  inputSplitSizePanel = widget_base(inputOptionsPanel, /row, uname = 'inputSplitSizePanel',  sensitive = 0)
  InputSplitSizeLabel = widget_label(inputSplitSizePanel, uname='InputSplitSizeLabel'  $
    ,scr_xsize = label_size $
    ,/align_left ,value='New image size:')

  InputSplitSizeText = widget_text(inputSplitSizePanel, uname='InputSplitSizeText' $
    , value = '1024' $
    , /editable ,/all_events)
    
  inputSplitMaxSizelabel = widget_label(inputSplitSizePanel, value = '', uname = 'inputSplitMaxSizeLabel', /dynamic_resize)
  
  ; Input list file name fields
  inputListNamePanel = widget_base(inputOptionsPanel, /row, uname = 'inputListNamePanel')
  InputListNameLabel = Widget_Label(inputListNamePanel, uname='InputListNameLabel'  $
    ,scr_xsize = label_size $
    ,/align_left ,value='Input List File:')

  InputListNameText = widget_text(inputListNamePanel, uname='InputListNameText' $
    , scr_xsize=text_size + 35   $
    ,/editable ,/all_events)

  BrowseInputListButton = widget_button(inputListNamePanel, uname='BrowseInputListButton' $
    ,/align_center ,value='...')

  ; Selection of the wavelet parameters
  WaveletBatchOptionsPanel = widget_base(wvbatch_form, uname='WaveletBatchOptionsPanel' ,frame=1  $
    ,title='IDL', /col)

  WaveletBatchDetailsPane = widget_base(WaveletBatchOptionsPanel, /col)

  WaveletBatchLabel = widget_label(WaveletBatchDetailsPane, uname='WaveletBatchLabel'  $
    , /align_left, value='Wavelet Options')

  FamilyBatchPanel = widget_base(WaveletBatchDetailsPane, col=2 $
    , XPAD=20 $
    )

  FamilyBatchLabel = widget_label(FamilyBatchPanel, uname='FamilyBatchLabel'  $
    , scr_xsize=40 $
    ,/align_left ,value='Family:')

  FamilyBatchComboBox = widget_combobox(FamilyBatchPanel, uname='FamilyBatchComboBox'  $
    ,value=[ 'Coiflet', 'Daubechies', 'Haar', 'Symlet' ])

  OrderBatchPanel = widget_base(WaveletBatchDetailsPane, col=2 $
    , xpad=20 $
    )

  OrderBatchLabel = widget_label(OrderBatchPanel, uname='OrderBatchLabel'  $
    , scr_xsize=40 $
    ,/align_left ,value='Order:')

  OrderBatchText = widget_text(OrderBatchPanel, value='1', uname='OrderBatchText'  $
    ,/editable ,/all_events)

  LevelsBatchPanel = widget_base(WaveletBatchDetailsPane, col=3 $
    , xpad=20 $
  )

  LevelsBatchLabel = widget_label(LevelsBatchPanel, uname='LevelsBatchLabel'  $
    , scr_xsize=40 $
    ,/align_left ,value='Levels:')

  LevelsBatchText = widget_text(LevelsBatchPanel, uname='LevelsBatchText'  $
    ,/editable)

  ; Add a check box to toggle creation of a single level or all levels
  CheckBoxBatchBase = widget_base(LevelsBatchPanel, uname='CheckBoxBatchBase' $
    ,title='All levels option select' ,column = 1 ,/nonexclusive)

  AllLevelsBatchToggle = widget_button(CheckBoxBatchBase, uname='AllLevelsBatchToggle'  $
    ,/align_left ,value='Create all levels')

  SpectrumBatchOutputPanel = widget_base(wvbatch_form, uname='SpectrumBatchOutputPanel' ,frame=1  $
    , /col $
    ,title='IDL')

  SpectrumBatchOutputLabel = widget_label(SpectrumBatchOutputPanel, uname='SpectrumBatchOutputLabel'  $
    ,/align_left ,value='Spectrum Options') ; Output Filenames')

  SpectrumBatchDetailsPanel = widget_base(SpectrumBatchOutputPanel, /row $
    , xpad=20 $
  )

  CoefficientBatchLabel = widget_label(SpectrumBatchDetailsPanel, uname='CoefficientBatchLabel'  $
    ,/align_left ,value='Coefficients')

  CoefficientBatchText = widget_text(SpectrumBatchDetailsPanel, uname='CoefficientBatchText'  $
    , scr_xsize=text_size - 32  $
    ,/editable)

  BrowseBatchMultiresButton = widget_button(SpectrumBatchDetailsPanel,  $
    uname='BrowseBatchMultiresButton' $
    ,/align_center ,value='Browse', tooltip='Specify folder to store all coefficients')

  calcBatchButtonPanel = widget_base(SpectrumBatchOutputPanel, /align_right, /row, xpad=20)

  CalcBatchButton = widget_button(calcBatchButtonPanel, uname='CalcBatchButton' $
    ,value='Calculate', Tooltip='Calculate the wavelet decomposition')

  widget_control, CoefficientBatchLabel, xsize = label_size

  ; default family selection = Haar
  widget_control, FamilyBatchComboBox, set_combobox_select = 2

; End of batch page
;-------------
end

pro Wavelet, Event
  statevalue={parent:         long(0), $
      inputName:      long(0), $
      inputID:      long(0), $    ; the file ID of the original image
      invoutput:      long(0), $    ; the file to store the inverse tranformation
            crystaloutput:    long(0), $    ; the file to store the extraced crystal
            crystalID:      long(-1), $   ; the crystal selection for extraction
            icrystalID:     long(-1), $   ; the crystal selection for inverse transform
            crystals:     ptr_new(), $  ; a array of bounding boxes for the crystals
            image:        ptr_new(), $  ; a reference to the decomposed imaged
            coefdirection:    long(0), $    ; 0 = output, 1 = input
            Coefficientoutput:  long(0), $    ; name of the coefficient file
      coefID:       long(0), $    ; file ID of coefficient file
            levels:       long(0), $
            calcAllLevels:    long(0), $  ; 0: only calculate for level in 'levels'
                            ; 1: calculate for all levels upto 'levels'
            thresholdID:    long(0), $
            threshText:     long(0), $
            order:        long(0), $
            family:       long(0), $
  ; wavelet batch page options
            inputList:      long(0), $
            batch_coef_output:  long(0), $
            wSEED:0L}

; Resolve_Routine, 'WaveletGUI_event',/COMPILE_FULL_FILE  ; Load event callback routines
;
; ; make sure we use the envi libraries
; envi_batch
;
  ; constants
  text_size = 315
  text_ch_size = 58

  ; Form definition
  Wave_form = Widget_Base( UNAME='Wave_form', /col  $
    ,TITLE='Wavelet coefficients')

  wavelet_tabs = widget_tab(wave_form, uname="wavelet_tabs")

  ; define first tab-page (interactive)
  wvlet_form = widget_base(wavelet_tabs, /col $
    , title = "Wavelet" $
  )

  ; define second tab-page (batch processing)
  wvbatch_form = widget_base(wavelet_tabs, /col $
    , title = "Batch" $
  )

;  ; define third tab-page (Image fusion)
;  wvfuse_form = widget_base(wavelet_tabs, /col $
;    , title = "Fusion" $
;  )

  waveletgui_create_batchPage, wvbatch_form, text_size
  
;  waveletgui_create_fusionPage, wvfuse_form, text_ch_size

  ; Create the contents of the first page
  ; Selection of the input image data
  inputPanel = widget_base(wvlet_form, /row, Title = "Wavelet")

  InputNameLabel = Widget_Label(inputPanel, UNAME='InputNameLabel'  $
    ,SCR_XSIZE=67 $
    ,/ALIGN_LEFT ,VALUE='Input File:')

  InputNameText = Widget_Text(inputPanel, UNAME='InputNameText' $
    , SCR_XSIZE=text_size + 35   $
    ,/EDITABLE ,/ALL_EVENTS)

  BrowseInputButton = Widget_Button(inputPanel, UNAME='BrowseInputButton' $
    ,/ALIGN_CENTER ,VALUE='...')

  ; Selection of the wavelet parameters
  WaveletOptionsPanel = Widget_Base(wvlet_form, UNAME='WaveletOptionsPanel' ,FRAME=1  $
    ,TITLE='IDL', /col)

  WaveletDetailsPane = widget_base(WaveletOptionsPanel, /col)

  WaveletLabel = Widget_Label(WaveletDetailsPane, UNAME='WaveletLabel'  $
    , /ALIGN_LEFT, VALUE='Wavelet Options')

  FamilyPanel = widget_base(WaveletDetailsPane, col=2 $
    , XPAD=20 $
    )

  FamilyLabel = Widget_Label(FamilyPanel, UNAME='FamilyLabel'  $
    , SCR_XSIZE=40 $
    ,/ALIGN_LEFT ,VALUE='Family:')

  FamilyComboBox = Widget_ComboBox(FamilyPanel, UNAME='FamilyComboBox'  $
    ,VALUE=[ 'Coiflet', 'Daubechies', 'Haar', 'Symlet' ])

  OrderPanel = widget_base(WaveletDetailsPane, col=2 $
    , XPAD=20 $
    )

  OrderLabel = Widget_Label(OrderPanel, UNAME='OrderLabel'  $
    , SCR_XSIZE=40 $
    ,/ALIGN_LEFT ,VALUE='Order:')

  OrderText = Widget_Text(OrderPanel, VALUE='1', UNAME='OrderText'  $
    ,/EDITABLE ,/ALL_EVENTS)

  LevelsPanel = widget_base(WaveletDetailsPane, col=3 $
    , XPAD=20 $
  )

  LevelsLabel = Widget_Label(LevelsPanel, UNAME='LevelsLabel'  $
    , SCR_XSIZE=40 $
    ,/ALIGN_LEFT ,VALUE='Levels:')

  LevelsText = Widget_Text(LevelsPanel, UNAME='LevelsText'  $
    ,/EDITABLE)

  ; Add a check box to toggle creation of a single level or all levels
  CheckBoxBase = Widget_Base(LevelsPanel, UNAME='CheckBoxBase' $
    ,TITLE='All levels option select' ,COLUMN = 1 ,/NONEXCLUSIVE)

  AllLevelsToggle = Widget_Button(CheckBoxBase, UNAME='AllLevelsToggle'  $
    ,/ALIGN_LEFT ,VALUE='Create all levels')

  SpectrumOutputPanel = Widget_Base(wvlet_form, UNAME='SpectrumOutputPanel' ,FRAME=1  $
    , /col $
    ,TITLE='IDL')

  SpectrumOutputLabel = Widget_Label(SpectrumOutputPanel, UNAME='SpectrumOutputLabel'  $
    ,/ALIGN_LEFT ,VALUE='Spectrum Options') ; Output Filenames')

  SpectrumDetailsPanel = widget_base(SpectrumOutputPanel, /row $
    , XPAD=20 $
  )

  CoefficientLabel = Widget_Label(SpectrumDetailsPanel, UNAME='CoefficientLabel'  $
    ,/ALIGN_LEFT ,VALUE='Coefficients')

  CoefficientText = Widget_Text(SpectrumDetailsPanel, UNAME='CoefficientText'  $
    , SCR_XSIZE=text_size - 60  $
    ,/EDITABLE)

  BrowseLoadMultiresButton = Widget_Button(SpectrumDetailsPanel,  $
    UNAME='BrowseLoadMultiresButton' $
    ,/ALIGN_CENTER ,VALUE='Load', tooltip='Load existing coefficients')

  BrowseMultiresButton = Widget_Button(SpectrumDetailsPanel,  $
    UNAME='BrowseMultiresButton' $
    ,/ALIGN_CENTER ,VALUE='New', tooltip='Specify new name for coefficients')

  calcButtonPanel = widget_base(SpectrumOutputPanel, /align_right, /row, xpad=20)

  StatisticsButton = Widget_Button(calcButtonPanel, UNAME='StatisticsButton' $
    , sensitiv = 0 $
    ,VALUE='Statistics', Tooltip='Display the wavelet decomposition statistics')

  SurfaceButton = Widget_Button(calcButtonPanel, UNAME='SurfaceButton' $
    , sensitiv = 0 $
    ,VALUE='Surface', Tooltip='Display the wavelet decomposition as 3D surface')

  CalcButton = Widget_Button(calcButtonPanel, UNAME='CalcButton' $
    ,VALUE='Calculate', Tooltip='Calculate the wavelet decomposition')

  ; Define the panel for extracting the crystal
  ExtractCrystalBase = widget_base(wvlet_form, UNAME='CrystalOutputPanel' ,FRAME=1, /col $
    , SENSITIVE = 0 $
  )

  CrystalLabel = Widget_Label(ExtractCrystalBase, UNAME='Select Crystal'  $
    , /ALIGN_LEFT ,VALUE='Select Crystal')

  crystalSelectPanel = widget_base(ExtractCrystalBase, /row $
    , XPAD=20 $
  )

  crystalSelectLabel = widget_label(crystalSelectPanel, uname='Crystal Number' $
    , value='Crystal Number' $
  )

  CrystalComboBox = Widget_ComboBox(crystalSelectPanel, UNAME='CrystalComboBox'  $
    ,VALUE=[ 'none' ])

  crystalFilePanel = widget_base(ExtractCrystalBase, /row $
    , XPAD=20 $
  )

  CrystalFileLabel = Widget_Label(crystalFilePanel, UNAME='CrystalFileLabel'  $
    ,/ALIGN_LEFT ,VALUE='Crystal File')

  CrystalFileText = Widget_Text(crystalFilePanel, UNAME='CrystalFileText'  $
    , SCR_XSIZE=text_size   $
    ,/EDITABLE $
  )

  BrowseCrystalFileButton = Widget_Button(crystalFilePanel,  $
    UNAME='BrowseCrystalFileButton' $
    ,/ALIGN_CENTER ,VALUE='...')

  GetCrystalButtonPanel = widget_base(ExtractCrystalBase, /align_right, /row $
    , XPAD=20 $
  )

  GetCrystalButton = Widget_Button(GetCrystalButtonPanel, UNAME='ExtractCrystalButton'  $
    ,TOOLTIP='Extract the crystal into a separate file' ,VALUE='Extract Crystal')

  ; Define the panel for reverse wavelet transform of the selected crystal
  InverseCrystalBase = widget_base(wvlet_form, UNAME='InverseCrystalOutputPanel' ,FRAME=1, /col $
    , SENSITIVE = 0 $
  )

  iCrystalLabel = Widget_Label(InverseCrystalBase, UNAME='InverseTransformCrystal'  $
    , /ALIGN_LEFT ,VALUE='Inverse Transform Crystal')

  InverseCrystalSelectPanel = widget_base(InverseCrystalBase, /row $
    , XPAD=20 $
  )

  icrystalSelectLabel = widget_label(InverseCrystalSelectPanel, uname='iCrystalNumber' $
    , value='Crystal Number' $
  )

  InverseCrystalComboBox = Widget_ComboBox(InverseCrystalSelectPanel, UNAME='InverseCrystalComboBox'  $
    ,VALUE=[ 'none' ])

  InverseThresholdPanel = widget_base(InverseCrystalBase, /row $
    , XPAD=20 $
  )

  InverseThresholdLabel = Widget_Label(InverseThresholdPanel, UNAME='InverseThresholdLabel'  $
    ,/ALIGN_LEFT ,VALUE='Threshold')

  InverseThresholdText = Widget_Text(InverseThresholdPanel, UNAME='InverseThresholdText'  $
    ,/EDITABLE $
  )

  InverseThresholdButton = Widget_Button(InverseThresholdPanel  $
    , UNAME='InverseThresholdButton' $
    , TOOLTIP='Count the number of coefficients below threshold' $
    ,/ALIGN_CENTER ,VALUE='Count')

  InverseThresholdCountLabel = Widget_Label(InverseThresholdPanel, UNAME='InverseThresholdCountLabel'  $
    , scr_xsize = 170 $
    ,/ALIGN_LEFT ,VALUE='abc')

  inverseCrystalFilePanel = widget_base(InverseCrystalBase, /row $
    , XPAD=20 $
  )

  InverseCrystalFileLabel = Widget_Label(inverseCrystalFilePanel, UNAME='InverseCrystalFileLabel'  $
    ,/ALIGN_LEFT ,VALUE='Inverse File')

  InverseCrystalFileText = Widget_Text(inverseCrystalFilePanel, UNAME='InverseCrystalFileText'  $
    , SCR_XSIZE=text_size   $
    ,/EDITABLE $
  )

  InverseCrystalFileButton = Widget_Button(inverseCrystalFilePanel,  $
    UNAME='InverseCrystalFileButton' $
    ,/ALIGN_CENTER ,VALUE='...')

  inverseButtonPanel = widget_base(InverseCrystalBase, /align_right, /row $
    , XPAD=20 $
  )

  InvTransformButton = Widget_Button(inverseButtonPanel, UNAME='InvTransformButton'  $
    ,TOOLTIP='Inverse transformation of the selected crystal' ,VALUE='Inverse Transform')

  ; align text boxes
  xsize = widget_info(CoefficientLabel, string_size = 'Coefficients')
  clabel_xsize = widget_info(crystalSelectLabel, string_size = 'Crystal Number')
  cryst_xsize = widget_info(CrystalFileLabel, string_size = 'Crystal File')
  icryst_xsize = widget_info(InverseCrystalFileLabel, string_size = 'Inverse File')
  label_size = xsize[0]
  if clabel_xsize[0] gt label_size then label_size = clabel_xsize[0]
  if cryst_xsize[0] gt label_size then label_size = cryst_xsize[0]
  if icryst_xsize[0] gt label_size then label_size = icryst_xsize[0]
  widget_control, CoefficientLabel, xsize = label_size
  widget_control, crystalSelectLabel, xsize = label_size
  widget_control, CrystalFileLabel, xsize = label_size
  widget_control, InverseCrystalFileLabel, xsize = label_size
  widget_control, InverseThresholdLabel, xsize = label_size

  ; default family selection = Haar
  widget_control, FamilyComboBox, set_combobox_select = 2

; End of wavelet page
;-------------

  closeButtonPanel = widget_base(Wave_form, /align_right, /col)
  CloseButton = Widget_Button(closeButtonpanel, UNAME='CloseButton'  $
    ,TOOLTIP='Close the window' ,VALUE='Close')

  statevalue.inputName=inputNameText
  statevalue.Coefficientoutput=CoefficientText
  statevalue.crystaloutput=CrystalFileText
  statevalue.invoutput =InverseCrystalFileText
  statevalue.crystalID = CrystalComboBox
  statevalue.order=orderText
  statevalue.family=familyComboBox
  statevalue.levels=levelsText
  statevalue.thresholdID = InverseThresholdCountLabel
  statevalue.threshText = InverseThresholdText

  ; save the current form state ...
  widget_control, Wave_form, set_uvalue = statevalue

  ;  ... and create the form
  Widget_Control, /REALIZE, Wave_form

  XManager, 'WaveletGUI', Wave_form, /NO_BLOCK

end
