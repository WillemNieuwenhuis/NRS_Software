;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleClose, Event
	; Close the application
	widget_control, event.top, /destroy

	; close the surface window if any
    catch, error
    if (error ne 0) then begin
		; no more error checks needed
		catch, /cancel

		return
	endif

    ; remove the previously displayed surface window, if any
	wshow, 1, 0

end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleCalculate, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.inputName, get_value=filename
	widget_control, state.coefficientoutput, get_value=mres
	widget_control, state.order, get_value=orderString
	widget_control, state.levels, get_value=levelString
	widget_control, state.family, get_value=family
	family = widget_info(state.family, /combobox_gettext)
	id = widget_info(event.top, FIND_BY_UNAME = 'AllLevelsToggle')
	calcAll = widget_info(id, /BUTTON_SET)

	order = 0

	on_ioerror, bad_order
	order = fix(orderString)
	on_ioerror, bad_level
	levels_arr = fix(levelString)
	levels = levels_arr[0]
	names = strarr(1)

	if (strlen(mres) eq 0) then begin
		ans = Dialog_Message('Filename for output coefficients is missing', /error, title='Wavelet error')
		return
	endif

	; reset error_handler
	on_ioerror, NULL

	envi_file_query, state.inputID, fname = filename

	; calculate the wavelet coefficients
	folder = '-1'	; not used in single output case
	wavelet_calc, filename, mres, folder, family, order, levels, calcAll, names, inter, crystals

	if n_elements(names) gt 0 then begin
		; fill the extract crystal combobox
		csel = widget_info(event.top, FIND_BY_UNAME='CrystalComboBox')
		widget_control, csel, set_value = names
		state.crystalID = 0	; preselect the first crystal

		; fill the inverse transform crystal combobox
		len = n_elements(inter[*, 0])
		determineAllCrystalNames, len, levels, inames
		csel = widget_info(event.top, FIND_BY_UNAME='InverseCrystalComboBox')
		widget_control, csel, set_value = inames
		state.icrystalID = 0	; preselect the first crystal

		if ptr_valid(state.image) then ptr_free, state.image
		if ptr_valid(state.crystals) then ptr_free, state.crystals
		state.crystals = ptr_new(crystals, /no_copy)
		state.image = ptr_new(inter, /no_copy)

		; make the extract crystal pane visible
		cpanel = widget_info(event.top, FIND_BY_UNAME='CrystalOutputPanel')
		widget_control, cpanel, sensitive = 1
		; make the inverse transform crystal pane visible
		cpanel = widget_info(event.top, FIND_BY_UNAME='InverseCrystalOutputPanel')
		widget_control, cpanel, sensitive = 1

		; enable the surface and statistics buttons
		surf_but = widget_info(event.top, FIND_BY_UNAME='SurfaceButton')
		widget_control, surf_but, sensitive = 1
		surf_but = widget_info(event.top, FIND_BY_UNAME='StatisticsButton')
		widget_control, surf_but, sensitive = 1

		widget_control, event.top, set_uvalue=state
	endif

	goto, done

bad_order:
	if (order le 0) then begin
		ans = Dialog_Message('Order must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

bad_level:
	if (levels le 0) then begin
		ans = Dialog_Message('Level must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

done:
	ans = Dialog_Message('Wavelet calculation finished', /information, title='Wavelet Information')

end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleCrystalFile, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.crystaloutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter filename for crystal output', file=filename, /write)
	if strlen(filename) eq 0 then return

	widget_control, state.crystaloutput, set_value=filename
	widget_control, event.top, set_uvalue=state
end

pro handleInverseCrystalFile, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.invoutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter filename for inverse transform output', file=filename, /write)
	if strlen(filename) eq 0 then return

	widget_control, state.invoutput, set_value=filename
	widget_control, event.top, set_uvalue=state
end

; Handle change of the crystal names combobox selection
; for the crystal extraction
pro handleCrystalChange, Event
	widget_control, event.top, get_uvalue=state

	state.crystalID = event.index
	widget_control, event.top, set_uvalue=state
end

; Handle change of the crystal names combobox selection
; for the reverse transfomation
pro handleInverseCrystalChange, Event
	widget_control, event.top, get_uvalue=state

	state.icrystalID = event.index
	widget_control, event.top, set_uvalue=state
end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleCrystalExtract, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.crystaloutput, get_value=crystname

	if strlen(crystname[0]) eq 0 then return
	if ~ptr_valid(state.crystals) then return
	if ~ptr_valid(state.image) then return

	csel = widget_info(event.top, FIND_BY_UNAME='CrystalComboBox')
	ctext = widget_info(csel, /combobox_gettext)
	cindex = state.crystalID
	cname = crystname[0] + '_' + ctext

	; extract the selected crystal
	crBounds = *(state.crystals)
	crystals = *(state.image)
	bounds = crBounds[*, cindex]
	left  = bounds[0]
	top   = bounds[1]
	right = bounds[2]
	bottom = bounds[3]
	selCrystal = crystals[left:right, top:bottom]

	envi_write_envi_file, selCrystal, out_name=cname
end

pro handleThresholdCount, event
	widget_control, event.top, get_uvalue = state
	widget_control, state.order, get_value = orderString
	widget_control, state.levels, get_value = levelString
	widget_control, state.thresholdID, get_value = countLabel
	widget_control, state.threshText, get_value = thresholdString
	family = widget_info(state.family, /combobox_gettext)

	order = 0
	threshold = -1.0

	on_ioerror, bad_order
	order = fix(orderString)
	on_ioerror, bad_level
	levels_arr = fix(levelString)
	levels = levels_arr[0]
	on_ioerror, bad_thresh
	th = float(thresholdString)
	threshold = th[0]

bad_thresh:
	if (abs(threshold) lt 0.0001) then begin
		widget_control, state.thresholdID, set_value = 'N/A'

		; reset error_handler
		on_ioerror, NULL

		return
	endif

	; reset error_handler
	on_ioerror, NULL

	; check availability of internal structures
	if ~ptr_valid(state.crystals) then return
	if ~ptr_valid(state.image) then return

	; calculate the multiresolution spectrum coefficients
	; reload original image
	ENVI_FILE_QUERY, state.inputID, nl = lines, ns = columns
	dims = [-1, 0, columns-1, 0, lines-1]
	data = ENVI_GET_DATA(fid=state.inputID, dims=dims, pos=0)

	; get the selected crystal
	csel = widget_info(event.top, FIND_BY_UNAME='InverseCrystalComboBox')
	ctext = widget_info(csel, /combobox_gettext)
	cindex = state.icrystalID
	levels = cindex / 4
	lv = cindex mod 4
	index = levels * 3 + lv
	if lv eq 3 then begin
		index = index -1
	endif
	levels = levels + 1

	; extract the selected crystal
	crBounds = *(state.crystals)
	bounds = crBounds[*, index]
	if lv eq 3 then begin
		left  = 0
		top   = 0
		right = bounds[0] - 1
		bottom = bounds[1] - 1
	endif else begin
		left  = bounds[0]
		top   = bounds[1]
		right = bounds[2]
		bottom = bounds[3]
	endelse

	func = 'wv_fn_' + family
	info = call_function(func, order, wavelet, scaling, ioff, joff)
	; get the coefficients for this level
	coef = wv_dwt(data, wavelet, scaling, ioff, joff, n_levels=levels)

	list = where(coef lt threshold, count)
	list = where(coef[left:right, top:bottom] lt threshold, count_cryst)
	percent = 100.0 * count / (lines * columns)
	perc_cryst = 100.0 * count_cryst / (lines * columns)

	out_str = string(count, format='(-i0)') + ' (' + string(percent, format='(f5.2)') + '%), ' + $
			  string(count_cryst, format='(-i0)') + ' (' + string(perc_cryst, format='(f5.2)') + '%)'
	widget_control, state.thresholdID, set_value = out_str

	goto, done

bad_order:
	if (order le 0) then begin
		ans = Dialog_Message('Order must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

bad_level:
	if (levels le 0) then begin
		ans = Dialog_Message('Level must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

done:
;	ans = Dialog_Message('Coefficient counting finished', /information, title='Wavelet Information')

end

;-----------------------------------------------------------------
; Take a selected crystal (from the UI) and perform a reverse
; transformation on that crystal.
; Needed info:
;	- original image (file ID available in memory)
;	- crystal indices (available in memory)
;	- selected crystal (available in the UI)
;	- wavelet family (available in the UI)
;	- wavelet order (available in the UI)
; Wavelet level is determined from the selected crystal
;-----------------------------------------------------------------
pro handleCrystalReverseTransform, event
	widget_control, event.top, get_uvalue = state
	widget_control, state.invoutput, get_value = crystname
	widget_control, state.order, get_value = orderString
	widget_control, state.levels, get_value = levelString
	widget_control, state.threshText, get_value = thresholdString
	family = widget_info(state.family, /combobox_gettext)

	order = 0
	threshold = 0

	on_ioerror, bad_order
	order = fix(orderString)
	on_ioerror, bad_level
	levels_arr = fix(levelString)
	levels = levels_arr[0]
	on_ioerror, bad_thresh
	th = float(thresholdString)
	threshold = th[0]

bad_thresh:
	if (threshold lt 0) then begin
		threshold = 0.0
	endif


	; reset error_handler
	on_ioerror, NULL

	; check availability of internal structures
	if strlen(crystname[0]) eq 0 then return
	if ~ptr_valid(state.crystals) then return
	if ~ptr_valid(state.image) then return

	; calculate the multiresolution spectrum coefficients
	; reload original image
	ENVI_FILE_QUERY, state.inputID, nl = lines, ns = columns
	dims = [-1, 0, columns-1, 0, lines-1]
	data = ENVI_GET_DATA(fid=state.inputID, dims=dims, pos=0)

	; get the selected crystal
	csel = widget_info(event.top, FIND_BY_UNAME='InverseCrystalComboBox')
	ctext = widget_info(csel, /combobox_gettext)
	cindex = state.icrystalID
	cname = crystname[0] + '_' + ctext + '_inv'
	levels = cindex / 4
	lv = cindex mod 4
	index = levels * 3 + lv
	if lv eq 3 then begin
		index = index -1
	endif
	levels = levels + 1

	; extract the selected crystal
	crBounds = *(state.crystals)
	bounds = crBounds[*, index]
	if lv eq 3 then begin
		left  = 0
		top   = 0
		right = bounds[0] - 1
		bottom = bounds[1] - 1
	endif else begin
		left  = bounds[0]
		top   = bounds[1]
		right = bounds[2]
		bottom = bounds[3]
	endelse

	func = 'wv_fn_' + family
	info = call_function(func, order, wavelet, scaling, ioff, joff)
	; get the coefficients for this level
	coef = wv_dwt(data, wavelet, scaling, ioff, joff, n_levels=levels)
	; temporarily save the selected crystal; then clear the array
	;  and re-insert the selected crystal
	selCrystal = coef[left:right, top:bottom]

	; remove all coefficient that are lower than the threshold
	sel_th = where(selCrystal lt threshold, cnt)
	if cnt gt 0 then begin
		selCrystal[sel_th] = 0
	endif

	coef[*] = 0
	coef[left:right, top:bottom] = selCrystal

	itrans = wv_dwt(coef, wavelet, scaling, ioff, joff, /inverse, n_levels=levels)

	envi_write_envi_file, itrans, out_name=cname

	goto, done

bad_order:
	if (order le 0) then begin
		ans = Dialog_Message('Order must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

bad_level:
	if (levels le 0) then begin
		ans = Dialog_Message('Level must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

done:
	ans = Dialog_Message('Inverse wavelet calculation finished', /information, title='Wavelet Information')

end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleCoefficientSurface, event
	widget_control, event.top, get_uvalue=state
	; test if we have a valid decomposition
	if ~ptr_valid(state.image) then return

    catch, error
    if (error ne 0) then begin
		; no more error checks needed
		catch, /cancel

		window, 1, Title = 'Wavelet coefficients'
	endif

	; set up a window, or show the current one
	wset, 1
	wshow, 1

	coef = *(state.image)

	shade_surf, coef, /xstyle, /ystyle, /zstyle $
		, title = 'Wavelet decomposition' $
		, xtitle = 'scale_x' $
		, ytitle = 'scale_y' $
		, ztitle = 'Coefficient'

end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleStatistics, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.levels, get_value=levelString

	; test if we have a valid decomposition
	if ~ptr_valid(state.image) then return

	on_ioerror, bad_level
	levels_arr = fix(levelString)
	levels = levels_arr[0]

	; reset error_handler
	on_ioerror, NULL

	coef = *(state.image)

	calcSummary_alt, coef, levels, levels, crystals, names, smyArray, smySubTotals
	summaryWindow, smyArray, smySubTotals, names

	goto, done

bad_level:
	if (levels le 0) then begin
		return
	endif

done:


end
;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro handleBrowseLoadCoefficient, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.coefficientoutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter existing coefficients filename', file=filename, /read)
	if strlen(filename) eq 0 then return

	state.coefdirection = 1

	ENVI_OPEN_FILE, filename, R_FID=fid, /no_realize
	state.coefID = fid
	ENVI_FILE_QUERY, state.coefID, nl = lines, ns = columns
	dims = [-1, 0, columns - 1, 0, lines - 1]
	data = ENVI_GET_DATA(fid = fid, dims = dims, pos = 0)
	if ptr_valid(state.image) then ptr_free, state.image
	state.image = ptr_new(data, /no_copy)

	; enable the surface and statistics buttons
	surf_but = widget_info(event.top, FIND_BY_UNAME='SurfaceButton')
	widget_control, surf_but, sensitive = 1
	surf_but = widget_info(event.top, FIND_BY_UNAME='StatisticsButton')
	widget_control, surf_but, sensitive = 1
	; but disable the calculate button
	surf_but = widget_info(event.top, FIND_BY_UNAME='CalcButton')
	widget_control, surf_but, sensitive = 0

	widget_control, state.coefficientoutput, set_value=filename
	widget_control, event.top, set_uvalue=state

	; try to determine the level from the coefficients filename
	pos = strpos(filename, '_bin')
	levstr = strmid(filename, pos - 3, 3)

	on_ioerror, bad_level
	levels_arr = fix(levstr)
	levels = levels_arr[0]
	widget_control, state.levels, set_value = strtrim(string(levels), 2)

	; now take care of filling the crystal selection combobox
	determineCrystals, columns, levels, crystals, names
	if n_elements(names) gt 0 then begin
		csel = widget_info(event.top, FIND_BY_UNAME='CrystalComboBox')
		widget_control, csel, set_value = names
		state.crystalID = 0	; preselect the first crystal
		; make the extract crystal pane visible
		cpanel = widget_info(event.top, FIND_BY_UNAME='CrystalOutputPanel')
		widget_control, cpanel, sensitive = 1

		if ptr_valid(state.crystals) then ptr_free, state.crystals
		state.crystals = ptr_new(crystals, /no_copy)
	endif

	; reset error_handler
	on_ioerror, NULL

  widget_control, event.top, set_uvalue=state

	goto, done

bad_level:
	if (pos eq -1) then begin
		ans = dialog_message('Could not determine the number of levels,\n make sure to set to correct value for valid statistics' $
			, title = 'Wavelet warning' $
			, /information $
			)
	end
done:
end

pro handleBrowseCoefficient, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.coefficientoutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Specify (new) filename for coefficients', file=filename, /write)
	if strlen(filename) eq 0 then return

	state.coefdirection = 0
	widget_control, state.coefficientoutput, set_value=filename
	widget_control, event.top, set_uvalue=state
end

; Prompt for the input file name
pro handleBrowseInput, Event
	widget_control, event.top, get_uvalue=state

	; disable the surface and statistics buttons
	surf_but = widget_info(event.top, FIND_BY_UNAME='SurfaceButton')
	widget_control, surf_but, sensitive = 0
	surf_but = widget_info(event.top, FIND_BY_UNAME='StatisticsButton')
	widget_control, surf_but, sensitive = 0
	; And enable the calculate button
	surf_but = widget_info(event.top, FIND_BY_UNAME='CalcButton')
	widget_control, surf_but, sensitive = 1

	envi_select, title = 'Enter input layer filename', fid = fid, /no_spec
	if fid eq -1 then return

	ENVI_FILE_QUERY, Fid, FNAME = filename, nb = inputlayers
	if (inputlayers ne 1) then begin
		ans = Dialog_Message('More than one layer found, using first layer', /information, title='Wavelet warning')
	endif

	cpanel = widget_info(event.top, FIND_BY_UNAME='CrystalOutputPanel')
	widget_control, cpanel, sensitive = 0

	cpanel = widget_info(event.top, FIND_BY_UNAME='InverseCrystalOutputPanel')
	widget_control, cpanel, sensitive = 0

	state.inputID = fid
	widget_control, state.inputName, set_value=filename
	widget_control, event.top, set_uvalue = state
end

pro handleToggleAllLevels, Event
	widget_control, event.top, get_uvalue=state

	isOn = 0
	if (event.id eq widget_info(event.top, FIND_BY_UNAME='AllLevelsToggle')) then begin
		isOn = widget_info(event.id, /BUTTON_SET)
	endif
	state.calcAllLevels = isOn

	widget_control, event.top, set_uvalue=state
end

