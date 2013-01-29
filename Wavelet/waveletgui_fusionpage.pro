; text_size in characters
pro waveletgui_create_fusionPage, wvfuse_form, text_size
; Create the contents of the fusion page
;-------------
  label_size = 80

  ; Selection of the input image data
  ; ---------------------------------
  inputOptionsPanel = widget_base(wvfuse_form, /col, /frame)
  
  hiresImageField = cw_dirfile(inputOptionsPanel, style='file' $
      , event_pro = 'wav_handleBrowseHiresImage' $
      , xsize = text_size $
      , title='Highres image:' $
      , uname = 'hiresImageField' $
      , /history)
      
  hiresImageInfoLabel = widget_label(inputOptionsPanel, uname = 'hiresImageInfoLabel' $
      , value = 'Dimensions:                      ')
  
  loresImageField = cw_dirfile(inputOptionsPanel, style='file' $
      , event_pro = 'wav_handleBrowseLoresImage' $
      , xsize = text_size $
      , title='Lowres image:' $
      , uname = 'loresImageField' $
      , /history)
  
  loresImageInfoLabel = widget_label(inputOptionsPanel, uname = 'loresImageInfoLabel' $
      , value = 'Dimensions:                      ')
      
  ; Selection of the wavelet parameters
  ; -----------------------------------
  WaveletFusionOptionsPanel = widget_base(wvfuse_form, uname='WaveletFusionOptionsPanel' ,frame=1  $
    ,title='IDL', /col)

  WaveletFusionDetailsPane = widget_base(WaveletFusionOptionsPanel, /col)

  WaveletFusionLabel = widget_label(WaveletFusionDetailsPane, uname='WaveletFusionLabel'  $
    , /align_left, value='Wavelet Options')

  FamilyFusionPanel = widget_base(WaveletFusionDetailsPane, col=2 $
    , xpad=20 $
    )

  FamilyFusionLabel = widget_label(FamilyFusionPanel, uname='FamilyFusionLabel'  $
    , scr_xsize=40 $
    ,/align_left ,value='Family:')

  FamilyFusionComboBox = widget_combobox(FamilyFusionPanel, uname='FamilyFusionComboBox'  $
    ,value=[ 'Coiflet', 'Daubechies', 'Haar', 'Symlet' ])

  OrderFusionPanel = widget_base(WaveletFusionDetailsPane, col=2 $
    , xpad=20 $
    )

  OrderFusionLabel = widget_label(OrderFusionPanel, uname='OrderFusionLabel'  $
    , scr_xsize=40 $
    ,/align_left ,value='Order:')

  OrderFusionText = widget_text(OrderFusionPanel, value='1', uname='OrderFusionText'  $
    ,/editable ,/all_events)

  ; Fusion output details
  ; ---------------------
  FusionOutputPanel = widget_base(wvfuse_form, uname='FusionOutputPanel' ,frame=1  $
    , /col $
    ,title='IDL')

  FusionOutputTitleLabel = widget_label(FusionOutputPanel, uname='FusionOutputTitleLabel'  $
    ,/align_left ,value='Fusion Options') ; Output Filenames')

  FusionDetailsPanel = widget_base(FusionOutputPanel, /col $
    , xpad=20 $
  )

  fusionOutputFilePanel = widget_base(FusionDetailsPanel, /row)
  FusionOutputLabel = widget_label(fusionOutputFilePanel, uname='FusionOutputLabel'  $
    ,/align_left ,value='Fusion output')

  FusionOutputText = widget_text(fusionOutputFilePanel, uname='FusionOutputText'  $
    , xsize=text_size - 8  $
    ,/editable)

  FusionOutputButton = widget_button(fusionOutputFilePanel,  $
    uname='FusionOutputButton' $
    ,/align_center ,value='New', tooltip='Specify name for fusion result')

  ; fusion parameters
  fusionParamsPanel = widget_base(FusionDetailsPanel, /col)
  values = ['Replace', 'Average']  
  fusionInjectType = cw_bgroup(fusionParamsPanel, values, /row, /exclusive, $
      uname = 'fusionInjectType', $
      label_left='Injection type  ', set_value=0)  
  
  fusionNormPanel = widget_base(fusionParamsPanel, /nonexclusive, /row)
  fusionNormalizeToggle = Widget_Button(fusionNormPanel, uname='fusionNormalizeToggle'  $
    ,/align_left ,value='Normalize')
  
  ; Calculate button
  fusionButtonPanel = widget_base(FusionOutputPanel, /align_right, /row, xpad=20)

  FusionCalcButton = widget_button(fusionButtonPanel, uname='FusionCalcButton' $
    ,value='Calculate', tooltip='Calculate the fusion')

  ; default family selection = Haar
  widget_control, FamilyFusionComboBox, set_combobox_select = 2

; End of fusion page
;-------------
end
