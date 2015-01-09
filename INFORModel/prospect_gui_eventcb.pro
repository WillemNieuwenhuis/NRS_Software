;
; IDL Event Callback Procedures
; prospect_gui_eventcb
;
pro prospect_gui_eventcb
end

pro handleBrowseSoilSpectrum, Event
	askFilename, Event

	handleRunCommand, Event
end

pro inform_handleAbout, Event
	inform_aboutwindow
end

pro handleDefaultSoilSpectrum, Event
	widget_control, event.top, get_uvalue = state

	val = widget_info(Event.id, /button_set)
	state.UseDefaultSoil = val
	if val eq 1 then begin
		widget_control, state.SoilSpectrumPanel, sensitive = 0
	endif else begin
		widget_control, state.SoilSpectrumPanel, sensitive = 1
	endelse
	widget_control, event.top, set_uvalue = state
end

pro askFilename, Event
	widget_control, event.top, get_uvalue = state
	widget_control, state.soilspectrum_input, get_value = filename

	filename = DIALOG_PICKFILE(title='Enter filename for coefficients', file = filename, /read)
	if strlen(filename) eq 0 then return

	widget_control, state.soilspectrum_input, set_value = filename
	widget_control, event.top, set_uvalue = state
end

pro inform_handleMeasuredSpectrumCheck, Event
	widget_control, event.top, get_uvalue = state

	val = widget_info(Event.id, /button_set)
	state.ShowMeasured = val
	if val eq 1 then begin
		widget_control, state.measuredPanel, sensitive = 1
	endif else begin
		widget_control, state.measuredPanel, sensitive = 0
	endelse

	widget_control, event.top, set_uvalue = state

	; update display only in case the measured spectrum is loaded
	; otherwise there is nothing to add or remove
	if state.measuredLoaded then begin
		handleRunCommand, event
	end

end

function inform_readSpectrum, filename, do_scaling
	data = READ_ASCII(filename, HEADER = header, DELIMITER = ',', DATA_START = 1 $
				, RECORD_START = 0 $
				, COUNT = count)

	df = fltarr(2, count)
	cols = n_elements(data.field1) / count
	if cols eq 1 then begin
		; spectrum frequencies not specified,
		; so generate the frequencies in the range from 400 - 2500 nm
		df[0,*] = 400 + 2100 * findgen(count) / (count - 1)
		mn = min(data.field1, max = mx)
		df[1, *] = data.field1
	end else begin
		mn = min(data.field1[cols - 1,*], max = mx)
		df = data.field1
	endelse
	if do_scaling eq 1 then $
		df[1, *] /= mx	; scale the spectrum to 0-100%

	return, df
end

pro inform_handleMeasuredSpectrumBrowse, Event
	widget_control, event.top, get_uvalue = state

	widget_control, state.measuredSpectrum, get_value = filename

	filename = DIALOG_PICKFILE(title='Enter filename for measured spectrum', file = filename, /read)
	if strlen(filename) eq 0 then return

	widget_control, state.measuredSpectrum, set_value = filename

	; (re)load the measure spectrum
	df = inform_readSpectrum(filename, 1)

	state.measSpectrum = ptr_new(df, /no_copy)
	state.measuredLoaded = 1

	widget_control, event.top, set_uvalue = state

	; make sure to update the display (remove old one, show new one)
	if state.ShowMeasured eq 1 then begin
		handleRunCommand, event
	end
end

pro inform_handleBrowseRefSpec, event
	fld_refspec = widget_info(event.top, find_by_uname = 'refSpectrumText')
	widget_control, fld_refspec, get_value = filename

	filename = DIALOG_PICKFILE(title='Enter filename for reference spectrum', file = filename, /read)
	if strlen(filename) eq 0 then return

	widget_control, fld_refspec, set_value = filename
end

pro conditional_plot, xaxis, values, ui_field, color
	val = widget_info(ui_field, /button_set)
	if val eq 1 then $
		oplot, xaxis, values, color = color
end

pro updateDisplays, event
	handleRunCommand, event
	inform_handleUpdateAngleGraph, event
	inform_handleUpdateSensitivityGraph, event
end

pro handleRunCommand, Event
	widget_control, event.top, get_uvalue = state
	widget_control, state.soilspectrum_input, get_value = filename

	if state.UseDefaultSoil eq 0 then begin
		; Read the soil_spectrum
		if strlen(filename) eq 0 then begin
			askFilename, Event
			widget_control, state.soilspectrum_input, get_value = filename
			if strlen(filename) eq 0 then begin
				ans = Dialog_Message('Specify filename for soil spectrum', /error, title='Prospect error')
				widget_control, state.soilspectrum_input, /input_focus
				return
			endif
		endif
		df = inform_readSpectrum(filename, 0)
		r_soil = df[1, *]
	endif else begin
		r_soil = default_soil_spectrum()
	endelse

	N      = getFloatNumber(state.leafStructure)
	Cab    = getFloatNumber(state.leafChlorophyl)
	Cw     = getFloatNumber(state.leafEqWater)
	Cm     = getFloatNumber(state.leafDryMatter)
	hot    = getFloatNumber(state.canopyHot)
	scale  = getFloatNumber(state.canopyScale)
	lai    = getFloatNumber(state.canopyLAI)
	lai_u  = getFloatNumber(state.canopyLAIU)
	sd     = getFloatNumber(state.canopyStemDensity)
	h      = getFloatNumber(state.canopyTreeHeight)
	cd     = getFloatNumber(state.canopyCrownDiam)
	ala    = getFloatNumber(state.canopyAvgLeafAngle)
	teta_s = getFloatNumber(state.externalTetaS)
	teta_o = getFloatNumber(state.externalTetaO)
	phi    = getFloatNumber(state.externalPhi)
	skyl   = getFloatNumber(state.externalSkyl)

	; do the calculation
	res = inform_prospect([N, Cab, Cw, Cm], $
				[hot, scale, lai, lai_u, sd, h, cd, ala], $
				teta_s, teta_o, phi, skyl, r_soil, $
				R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
				R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
				T_S = t_s, T_O = t_o $
			  )

	; Show the results
	widget_control, state.draw_pane, get_value = draw_pane
	wset, draw_pane
    DEVICE, DECOMPOSED = 0
	loadct,12, /silent	; 16 level color lut

	; Now start the plot window
	freq = indgen(421) * 5 + 400
	; clear plot area and redisplay the axes
	plot, freq $
		, BACKGROUND = 255, COLOR = 0 $
		, /nodata $
		, xrange = [400, 2500], /xstyle $
		, yrange = [0, 1], /ystyle $
		, r_leaf $	; first data
      	, Position = [0.15, 0.15, 0.85, 0.85] $
		, title = 'Spectral Reflectance Variation' $
      	, font = 0 $
      	, XTitle = 'Wavelength [nm]' $
      	, YTitle = 'Reflectance'

	conditional_plot, freq, r_forest, state.plot_r_forest, 24	; 24 = dark green
	conditional_plot, freq, r_soil_scale, state.plot_r_soil, 232	; 232 = light grey
	conditional_plot, freq, r_understorey, state.plot_r_under, 8	; 8 = dark dark green
	conditional_plot, freq, r_c_inf, state.plot_r_c_inf, 104	; 104 = blue
	conditional_plot, freq, r_leaf, state.plot_r_leaf, 40	; 40 = green
	conditional_plot, freq, t_leaf, state.plot_t_leaf, 88	; 88 = cyan
	conditional_plot, freq, t_s, state.plot_t_s, 120	; 120 = purple
	conditional_plot, freq, t_o, state.plot_t_o, 136	; 136 = pink
	conditional_plot, freq, G, state.plot_G, 216	; 216 = light red

	; handle the measured spectrum display
	if ptr_valid(state.measSpectrum) then begin
		ms = *(state.measSpectrum)
		if (state.ShowMeasured eq 1) and (state.measuredLoaded eq 1) then begin
			oplot, ms[0, *], ms[1, *], color = 200	; 200 = red
		end
	end

	widget_control, state.print_CO, set_value = string(co)
	widget_control, state.print_LAIc, set_value = string(co * lai)

end

function getIntNumber, ui_element
	widget_control, ui_element, get_value = value
	return, fix(value[0])
end

function getFloatNumber, ui_element
	widget_control, ui_element, get_value = value
	return, float(value[0])
end

pro handleReset, Event
	widget_control, event.top, get_uvalue = state

	resetToDefaults, state

	handleRunCommand, event	; update the display
end

pro resetToDefaults, state
	widget_control, state.leafStructure		, set_value = '2'
	widget_control, state.leafChlorophyl	, set_value = '60'
	widget_control, state.leafEqWater		, set_value = '0.025'
	widget_control, state.leafDryMatter		, set_value = '0.025'
	widget_control, state.canopyHot			, set_value = '0.02'
	widget_control, state.canopyScale		, set_value = '1'
	widget_control, state.canopyLAI			, set_value = '7'
	widget_control, state.canopyLAIU		, set_value = '0.1'
	widget_control, state.canopyStemDensity	, set_value = '650'
	widget_control, state.canopyTreeHeight	, set_value = '20'
	widget_control, state.canopyCrownDiam	, set_value = '4.5'
	widget_control, state.canopyAvgLeafAngle, set_value = '55'
	widget_control, state.externalTetaS		, set_value = '30'
	widget_control, state.externalTetaO		, set_value = '0'
	widget_control, state.externalPhi		, set_value = '0'
	widget_control, state.externalSkyl		, set_value = '0.1'
end

pro inform_handleUpdateAngleGraph, event
	widget_control, event.top, get_uvalue = state

	; determine the angle type for the model calculation
	angleType = widget_info(state.select_AngleCB, /combobox_gettext)

	useTeta_o = 1
	if angleType eq 'Observation zenith angle' then begin
		useTeta_o = 1
	endif else begin ; 'Sun zenith angle'
		useTeta_o = 0
	endelse

	; determine which reflectance value to plot
	reflectanceType = widget_info(state.reflectSelectComboBox, /combobox_gettext)
	useForestReflectance = 1
	useUnderstoreyReflectance = 0
	useCanopyReflectance = 0
	useLeafReflectance = 0
	if reflectanceType eq 'Forest' then useForestReflectance = 1
	if reflectanceType eq 'Understorey' then useUnderstoreyReflectance = 1
	if reflectanceType eq 'Infinite canopy' then useCanopyReflectance = 1
	if reflectanceType eq 'Leaf' then useLeafReflectance = 1

	if state.UseDefaultSoil eq 0 then begin
		; Read the soil_spectrum
		if strlen(filename) eq 0 then begin
			askFilename, Event
			widget_control, state.soilspectrum_input, get_value = filename
			if strlen(filename) eq 0 then begin
				ans = Dialog_Message('Specify filename for soil spectrum', /error, title='Prospect error')
				widget_control, state.soilspectrum_input, /input_focus
				return
			endif
		endif
		df = inform_readSpectrum(filename, 0)
		r_soil = df[1, *]
	endif else begin
		r_soil = default_soil_spectrum()
	endelse

	N      = getFloatNumber(state.leafStructure)
	Cab    = getFloatNumber(state.leafChlorophyl)
	Cw     = getFloatNumber(state.leafEqWater)
	Cm     = getFloatNumber(state.leafDryMatter)
	hot    = getFloatNumber(state.canopyHot)
	scale  = getFloatNumber(state.canopyScale)
	lai    = getFloatNumber(state.canopyLAI)
	lai_u  = getFloatNumber(state.canopyLAIU)
	sd     = getFloatNumber(state.canopyStemDensity)
	h      = getFloatNumber(state.canopyTreeHeight)
	cd     = getFloatNumber(state.canopyCrownDiam)
	ala    = getFloatNumber(state.canopyAvgLeafAngle)
	teta_s = getFloatNumber(state.externalTetaS)
	teta_o = getFloatNumber(state.externalTetaO)
	phi    = getFloatNumber(state.externalPhi)
	skyl   = getFloatNumber(state.externalSkyl)

	wave_sel = widget_info(state.wavelengthSelectList, /list_select)
	if n_elements(wave_sel) eq 1 then begin
		if wave_sel[0] lt 0 then return
	endif

	color_sel = wave_sel
	wave_sel = wave_sel * 20	; calculate the actual wavelength indices in the data arrays
	waveCount = n_elements(wave_sel)
	degree_step = 10	; for each degree_step the model is calculated
	degree_count = round(180 / degree_step) + 1	; number of elements in a graph
	graphs = fltarr(waveCount, degree_count)

	; accumulate reflectances for all selected wavelengths
	deg_index = 0
	for deg = -90, 90, degree_step do begin
		if useTeta_o eq 1 then begin
			teta_o = deg
		endif else begin
			teta_s = deg
		endelse

		; do the calculation
		res = inform_prospect([N, Cab, Cw, Cm], $
					[hot, scale, lai, lai_u, sd, h, cd, ala], $
					teta_s, teta_o, phi, skyl, r_soil, $
					R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
					R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
					T_S = t_s, T_O = t_o $
				  )
		if useForestReflectance eq 1 then begin
			graphs[*, deg_index] = r_forest[wave_sel]
		endif
		if useUnderstoreyReflectance eq 1 then begin
			graphs[*, deg_index] = r_understorey[wave_sel]
		endif
		if useCanopyReflectance eq 1 then begin
			graphs[*, deg_index] = r_c_inf[wave_sel]
		endif
		if useLeafReflectance eq 1 then begin
			graphs[*, deg_index] = r_leaf[wave_sel]
		endif
		deg_index += 1
	endfor

	; Show the results
	angles = indgen(degree_count) * degree_step - 90
	inform_drawAngle, state.draw_anglePane, angles, r_leaf, angleType, 12

	for wvi = 0, waveCount - 1 do begin
		oplot, angles, graphs[wvi, *], color = color_sel[wvi] * 10 + 8
	endfor
end

pro inform_handleUpdateSensitivityGraph, event
	widget_control, event.top, get_uvalue = state

	; determine which reflectance value to plot
	sensreflec = widget_info(event.top, find_by_uname = 'sensReflectSelectComboBox')
	reflectanceType = widget_info(sensreflec, /combobox_gettext)
	useForestReflectance = 1
	useUnderstoreyReflectance = 0
	useCanopyReflectance = 0
	useLeafReflectance = 0
	if reflectanceType eq 'Forest' then useForestReflectance = 1
	if reflectanceType eq 'Understorey' then useUnderstoreyReflectance = 1
	if reflectanceType eq 'Infinite canopy' then useCanopyReflectance = 1
	if reflectanceType eq 'Leaf' then useLeafReflectance = 1

	; get soil spectrum
	if state.UseDefaultSoil eq 0 then begin
		; Read the soil_spectrum
		if strlen(filename) eq 0 then begin
			askFilename, Event
			widget_control, state.soilspectrum_input, get_value = filename
			if strlen(filename) eq 0 then begin
				ans = Dialog_Message('Specify filename for soil spectrum', /error, title='Prospect error')
				widget_control, state.soilspectrum_input, /input_focus
				return
			endif
		endif
		df = inform_readSpectrum(filename, 0)
		r_soil = df[1, *]
	endif else begin
		r_soil = default_soil_spectrum()
	endelse

	; get the values for all model parameters
	N      = getFloatNumber(state.leafStructure)
	Cab    = getFloatNumber(state.leafChlorophyl)
	Cw     = getFloatNumber(state.leafEqWater)
	Cm     = getFloatNumber(state.leafDryMatter)
	hot    = getFloatNumber(state.canopyHot)
	scale  = getFloatNumber(state.canopyScale)
	lai    = getFloatNumber(state.canopyLAI)
	lai_u  = getFloatNumber(state.canopyLAIU)
	sd     = getFloatNumber(state.canopyStemDensity)
	h      = getFloatNumber(state.canopyTreeHeight)
	cd     = getFloatNumber(state.canopyCrownDiam)
	ala    = getFloatNumber(state.canopyAvgLeafAngle)
	teta_s = getFloatNumber(state.externalTetaS)
	teta_o = getFloatNumber(state.externalTetaO)
	phi    = getFloatNumber(state.externalPhi)
	skyl   = getFloatNumber(state.externalSkyl)

	sens_parm_fld = widget_info(event.top, find_by_uname = 'sensParameterCombobox')
	sens_parm = widget_info(sens_parm_fld, /combobox_gettext)
	parm_index = getIndexOfParam(sens_parm)
	p_min = getParameterField(parm_index, 3)
	p_max = getParameterField(parm_index, 4)

	sens_steps_fld = widget_info(event.top, find_by_uname = 'sensParameterRangeStepsText')
	sens_stepCount = getFloatNumber(sens_steps_fld)
	if sens_stepCount lt 2 then return

	sens_step = (float(p_max) - float(p_min)) / (sens_stepCount - 1)

	graphs = fltarr(sens_stepCount, 421)

	; accumulate reflectances for all selected wavelengths
	deg_index = 0
	for step = 0, sens_stepCount - 1 do begin
		update_model_input_params, parm_index, p_min, step, sens_step $
			, N, Cab, Cw, Cm, hot, scale, lai, lai_u, sd, cd, ala, teta_s, teta_o, phi, skyl

		; do the calculation
		res = inform_prospect([N, Cab, Cw, Cm], $
					[hot, scale, lai, lai_u, sd, h, cd, ala], $
					teta_s, teta_o, phi, skyl, r_soil, $
					R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
					R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
					T_S = t_s, T_O = t_o $
				  )
		if useForestReflectance eq 1 then begin
			graphs[deg_index, *] = r_forest
		endif
		if useUnderstoreyReflectance eq 1 then begin
			graphs[deg_index, *] = r_understorey
		endif
		if useCanopyReflectance eq 1 then begin
			graphs[deg_index, *] = r_c_inf
		endif
		if useLeafReflectance eq 1 then begin
			graphs[deg_index, *] = r_leaf
		endif
		deg_index += 1
	endfor

	; Show the results
	draw_pane = Widget_Info(event.top, find_by_uname='sensView')
  	Widget_Control, draw_pane, Get_Value = dispID
	wset, dispID
    DEVICE, DECOMPOSED = 0
	loadct,12, /silent	; 16 level color lut

	ymax = max(graphs, min=ymin, /nan)
	yr = (ymax - ymin)
	ymin = 1.0 * ymin - yr * 0.1
	ymax = 1.0 * ymax + yr * 0.1

	; Now start the plot window
	if n_elements(freq) eq 0 then $
		freq = indgen(421) * 5 + 400
	; display the axes
	plot, freq $
		, /nodata $
		, BACKGROUND = 255, COLOR = 0 $
		, xrange = [400, 2500], /xstyle $
		, xtickinterval = 400 $
		, yrange = [0.0, ymax], /ystyle $
;		, r_leaf $	; first data
      	, Position = [0.15, 0.15, 0.85, 0.85] $
		, title = 'Spectral Reflectance Variation' $
      	, font = 0 $
      	, XTitle = 'Wavelength [nm]' $
      	, YTitle = 'Reflectance'

	divs = max([3, min([12, sens_stepCount])])
	div_step = (float(p_max) - float(p_min)) / divs
	color_step = (247 - 16) / (sens_stepCount - 1)	; (247-16) to avoid last graph to be draw in background color
	for wvi = 0, sens_stepCount - 1 do begin
		draw_color = wvi * color_step + 8
		oplot, freq, graphs[wvi, *], color = draw_color
	endfor

	dec = floor(alog10(div_step))
	format = '(i4)'
	if dec lt 0 then format = '(f0.' + string(abs(dec), format='(i0)') + ')'
	ticks = findgen(divs + 1) * div_step + p_min
	tick_names = string(ticks, format = format)
	colorbar_n, position = [0.87, 0.15, 0.90, 0.85] $
		, ncolors = 240 $
		, BACKGROUND = 255, COLOR = 0 $
		,/vertical, /right, font = 0 $
		, divisions = divs, ticknames = tick_names
end

pro update_model_input_params, parm_index, p_min, step, sens_step $
		, N, Cab, Cw, Cm, hot, scale, lai, lai_u, sd, cd, ala, teta_s, teta_o, phi, skyl
	case parm_index of
		 0 : N = p_min + step * sens_step
		 1 : Cab = p_min + step * sens_step
		 2 : Cw = p_min + step * sens_step
		 3 : Cm = p_min + step * sens_step
		 4 : hot = p_min + step * sens_step
		 5 : scale = p_min + step * sens_step
		 6 : lai = p_min + step * sens_step
		 7 : lai_u = p_min + step * sens_step
		 8 : sd = p_min + step * sens_step
		 9 : h = p_min + step * sens_step
		10 : cd = p_min + step * sens_step
		11 : ala = p_min + step * sens_step
		12 : teta_s = p_min + step * sens_step
		13 : teta_o = p_min + step * sens_step
		14 : phi = p_min + step * sens_step
		15 : skyl = p_min + step * sens_step
	endcase
end

pro inform_handle_sensbox, event
	inform_update_minmax, event.top

	; also update the graph
	inform_handleUpdateSensitivityGraph, event
end

pro inform_update_minmax, top
	sens_parm_fld = widget_info(top, find_by_uname = 'sensParameterCombobox')
	sens_parm = widget_info(sens_parm_fld, /combobox_gettext)
	parm_index = getIndexOfParam(sens_parm)

	p_min = getParameterField(parm_index, 3)
	smin = widget_info(top, find_by_uname = 'sensParameterRangeMinText')
	widget_control, smin, set_value = string(p_min)

	p_max = getParameterField(parm_index, 4)
	smax = widget_info(top, find_by_uname = 'sensParameterRangeMaxText')
	widget_control, smax, set_value = string(p_max)
end

pro inform_handleMainTabChange, event
	cur_tab_fld = widget_info(event.top, find_by_uname = 'graphTab')
	cur_tab = widget_info(cur_tab_fld, /tab_current)
	if cur_tab eq 2 then $
		inform_handleUpdateSensitivityGraph, event
end

pro inform_handleInverseSens, event
	top = event.top

	paramCount = getParameterCount()
	activeCount = 0
	count = 0
	for p = 0, getParameterCount() - 1 do begin
		par_val = getParameterField(p, -1)
		if count eq 0 then $
			fix_array = par_val $
		else $
			fix_array = [fix_array, par_val]
		count++

		uname = 'chkbox_par_' + par_val.desg
		chkbox = widget_info(top, find_by_uname = uname)
		isset = widget_info(chkbox, /button_set)
		if isset eq 0 then continue

		uname = 'text_par_' + par_val.desg + '_min'
		txtmin = widget_info(top, find_by_uname = uname)
		par_val.min_val = getFloatNumber(txtmin)

		uname = 'text_par_' + par_val.desg + '_max'
		txtmax = widget_info(top, find_by_uname = uname)
		par_val.max_val = getFloatNumber(txtmax)

		if activeCount eq 0 then $
			par_array = par_val $
		else $
			par_array = [par_array, par_val]

		activeCount++
	endfor

	steps_text = widget_info(top, find_by_uname = 'steps_text')
	steps = getIntNumber(steps_text)
	fld_refspec = widget_info(top, find_by_uname = 'refSpectrumText')
	widget_control, fld_refspec, get_value = refspec_filename

	ref_comb = widget_info(top, find_by_uname = 'invSensReflectSelectComboBox')
	reflectanceType = widget_info(ref_comb, /combobox_gettext)

	ref = inform_readSpectrum(refspec_filename, 0)

	widget_control, event.top, get_uvalue = state
	widget_control, state.soilspectrum_input, get_value = filename

	if state.UseDefaultSoil eq 0 then begin
		; Read the soil_spectrum
		if strlen(filename) eq 0 then begin
			askFilename, Event
			widget_control, state.soilspectrum_input, get_value = filename
			if strlen(filename) eq 0 then begin
				ans = Dialog_Message('Specify filename for soil spectrum', /error, title='Prospect error')
				widget_control, state.soilspectrum_input, /input_focus
				return
			endif
		endif
		df = inform_readSpectrum(filename, 0)
		r_soil = df[1, *]
	endif else begin
		r_soil = default_soil_spectrum()
	endelse

	N      = getFloatNumber(state.leafStructure)
	Cab    = getFloatNumber(state.leafChlorophyl)
	Cw     = getFloatNumber(state.leafEqWater)
	Cm     = getFloatNumber(state.leafDryMatter)
	hot    = getFloatNumber(state.canopyHot)
	scale  = getFloatNumber(state.canopyScale)
	lai    = getFloatNumber(state.canopyLAI)
	lai_u  = getFloatNumber(state.canopyLAIU)
	sd     = getFloatNumber(state.canopyStemDensity)
	h      = getFloatNumber(state.canopyTreeHeight)
	cd     = getFloatNumber(state.canopyCrownDiam)
	ala    = getFloatNumber(state.canopyAvgLeafAngle)
	teta_s = getFloatNumber(state.externalTetaS)
	teta_o = getFloatNumber(state.externalTetaO)
	phi    = getFloatNumber(state.externalPhi)
	skyl   = getFloatNumber(state.externalSkyl)

	permCount = steps ^ activeCount
	fld_result = widget_info(top, find_by_uname = 'InverseModeResultText')
	widget_control, fld_result, set_value = 'Running ' + string(permCount, format = '(i0)') + ' permutations'

	fix_array = [N, Cab, Cw, Cm, hot, scale, lai, lai_u, sd, h, cd, ala, teta_s, teta_o, phi, skyl]
	optimal_params = inform_inverse_mode(reflectanceType, steps, ref, par_array, fix_array, r_soil)
	if n_elements(optimal_params) eq 1 then return

	lowest_error = optimal_params[n_elements(optimal_params) - 1]
	optimal_params = optimal_params[0:n_elements(optimal_params) - 2]

	range = floor(alog10(lowest_error))
	dec = 4 - range - 1
	if dec gt 0 then $
		err_format = '(f0.' + string(dec, format='(i0)') + ')' $
	else $
		err_format = '(i0)'

	widget_control, fld_result, set_value = 'Best parameter set'
	widget_control, fld_result, /append, set_value = ''
	widget_control, fld_result, /append, set_value = 'Overall error (lsq): ' + string(lowest_error, format = err_format)
	for p = 0, getParameterCount() - 1 do begin
		pv = getParameterField(p, -1)
		tval = valtostr(optimal_params[p], pv)
		res_str = string(pv.desg, format = '(a-8)') + ' = ' + string(tval, format = '(a8)')
		isActiveParam = checkActive(pv, par_array, rng_str)
		if isActiveParam ne 0 then $
			res_str += rng_str
		widget_control, fld_result, /append, set_value = res_str
	endfor
end

function checkActive, pv, par_array, rng_str
	name = pv.name
	for i = 0, n_elements(par_array) - 1 do begin
		p = par_array[i]
		isActive = p.name eq name
		if isActive then begin
			minval = valtostr(p.min_val, p)
			maxval = valtostr(p.max_val, p)
			rng_str = ' * in range (' + minval + ', ' + maxval + ')'
			return, isActive
		endif
	endfor

	return, 0
end
