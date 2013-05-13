pro SpectrumExtraction_define_buttons, buttonInfo

  envi_define_menu_button, buttonInfo, VALUE = 'Spectrum Extraction', $
    UVALUE = 'Spectrum Extraction', EVENT_PRO = 'SpectrumExtraction', $
    REF_VALUE = 'Spectral Derivative', POSITION = 'after',/SIBLING

end

; Main command line routine; can be started from the menu ("Spectral | Spectral Extraction")
pro SpectrumExtraction, Event
	; Select the hyper spectral images
	hymaps = ENVI_PICKFILE(title='Select multi-resolution images', /multiple_files)
	if (n_elements(hymaps) eq 1) then begin
		if (hymaps eq '') then begin
			return
		endif
	endif

	; Select the shape files
	shapes = ENVI_PICKFILE(title='Select shapefile point features', filter='*.shp', /multiple_files)
	if (n_elements(shapes) eq 1) then begin
		if (shapes eq '') then begin
			return
		endif
	endif

	; Select an output name for the (textual in CSV) results
	output = Dialog_Pickfile(title='Specify output CSV name', filter='*.csv', DEFAULT_EXTENSION='csv', /write, /overwrite_prompt)
	if (n_elements(output) eq 1) then begin
		if (output eq '') then begin
			return
		endif
	endif

	thresholdUI = GUI_PROMPT('Majority threshold (in %, 0 = no majority) ', $
	    TITLE='Type a number', $
	    VALIDATE_TYPE=size(0.0, /TYPE)) ;, $
	    ;XSIZE=30, $
	    ;XOFFSET=100, YOFFSET=100)
	threshold = float(thresholdUI) / 100

	; calculate the spectrum
	AverageSpectrum, shapes, hymaps, output, threshold

	ans = Dialog_Message('Finished', title='Spectrum Extraction', /information)

end