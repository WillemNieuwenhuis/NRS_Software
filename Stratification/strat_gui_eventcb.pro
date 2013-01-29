pro handle_strat_Close, Event
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
;	wshow, 1, 0

end

pro handleChangeLutType, event
	widget_control, event.top, get_uvalue=state
	strat_draw = widget_info(event.top, FIND_BY_UNAME='StratDraw')
	widget_control, strat_draw, get_value = draw_pane
	combo_id = widget_info(event.top, find_by_uname='lutTypeCombobox')

	lut_selected = widget_info(combo_id, /combobox_gettext)
	lut_id = 0	; black to white
	if lut_selected eq 'Rainbow' then lut_id = 39	; custom Rainbow (from get_yellow_rainbow_lut)

	wset, draw_pane
	; set the colour table
    DEVICE, DECOMPOSED = 0
    DEVICE, SET_FONT='Helvetica', /TT_FONT
    if lut_id ne 39 then begin
    	get_bw_lut, lut
		tvlct, lut[0,*], lut[1,*], lut[2,*]
    endif else begin
    	get_yellow_rainbow_lut, lut
		tvlct, lut[0,*], lut[1,*], lut[2,*]
	endelse

	draw_graph, event
end

; Prompt for the input file name
pro handle_strat_BrowseInput, Event
	widget_control, event.top, get_uvalue=state

again:
	envi_select, title = 'Enter input NDVI/RPD time series', fid = fid, /no_spec
	if fid eq -1 then return

	ENVI_FILE_QUERY, Fid, FNAME = filename, nb = inputlayers
	if (inputlayers eq 1) then begin
		ans = Dialog_Message('Only one layer found, need time series; try again?', /question, title='Stratification warning')
		if ans eq 'Yes' then begin
			goto, again
		end
		return
	endif

	state.inputID = fid
	widget_control, state.inputName, set_value=filename
	widget_control, event.top, set_uvalue = state
end

pro handle_strat_BrowseDEM, Event
	widget_control, event.top, get_uvalue=state

again:
	envi_select, title = 'Enter input DEM', fid = fid, /no_spec
	if fid eq -1 then return

	ENVI_FILE_QUERY, Fid, FNAME = filename, nb = inputlayers
	if (inputlayers gt 1) then begin
		ans = Dialog_Message('More than one layer found, Using first band', /information, title='Stratification warning')
	endif

;    linelabel = Widget_Info(event.top, FIND_BY_UNAME='HeightLineLabel')

	state.inputDEMID = fid
	widget_control, state.inputDEMName, set_value=filename
;	widget_control, linelabel, set_value = 'lines'
	widget_control, event.top, set_uvalue = state
end

pro handleBrowseHeightTable, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.heightTable, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter filename of height table', file=filename, /read)
	if strlen(filename) eq 0 then return

	widget_control, state.heightTable, set_value=filename
	widget_control, event.top, set_uvalue=state
end

pro handleSaveStratification, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.stratoutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter filename for stratification output', file=filename, /write)
	if strlen(filename) eq 0 then return

	widget_control, state.stratoutput, set_value=filename
	widget_control, event.top, set_uvalue=state
end

pro handleBrowseSavePicture, Event
	widget_control, event.top, get_uvalue=state
	widget_control, state.pictureOutput, get_value=filename

	filename = DIALOG_PICKFILE(title='Enter filename for picture output', file=filename, /write)
	if strlen(filename) eq 0 then return

	widget_control, state.pictureOutput, set_value=filename
	widget_control, event.top, set_uvalue=state
end

pro handleSavePicture, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.pictureOutput, get_value=filename

	if strlen(filename) eq 0 then return

	thisDrawing = !D.Name
	scale = state.draw_scale

	set_plot, 'Z'
	state.draw_scale = 5
	widget_control, event.top, set_uvalue=state
	Device, Set_Resolution=[3000,1500], Z_Buffer=0

	draw_graph, event

	img = tvrd()
	tvlct, r, g, b, /get
	device, z_buffer = 1

	set_plot, thisDrawing

	image24 = BytArr(3, 3000, 1500)
   	image24[0,*,*] = r[img]
   	image24[1,*,*] = g[img]
   	image24[2,*,*] = b[img]

	Write_JPEG, filename, image24, True=1, Quality=75

	state.draw_scale = scale
	widget_control, event.top, set_uvalue=state
end

pro draw_graph, event ; dem_min, dem_max, matrix, height_data, header: now in state struct
	strat_draw = widget_info(event.top, FIND_BY_UNAME='StratDraw')
	combo_id = widget_info(event.top, find_by_uname='lutTypeCombobox')

	widget_control, event.top, get_uvalue=state

	if ~ptr_valid(state.matrix) then return

	matrix = *(state.matrix)
	height_data = 0
	header = ''
	if ptr_valid(state.height_data) then begin
		height_data = *(state.height_data)
		header = state.header
	end
	dem_min = state.dem_min
	dem_max = state.dem_max

	plot_height = state.draw_height * state.draw_scale
	plot_width = state.draw_width * state.draw_scale
	legend_height = state.draw_height * state.draw_scale
	legend_width = state.legend_width * state.draw_scale

	character_size = 0.9 * state.draw_scale
	character_thick = 1.0 * state.draw_scale
	line_thick = 1.5 * state.draw_scale - 0.75
	symbol_size = state.draw_scale

	margin_left = 0.15
	margin_bottom = 0.15
	margin_height = 0.75
	margin_width = 0.72
	draw_height = margin_height * plot_height - 1
	draw_width = margin_width * plot_width -1
	freq = indgen(36)

	if n_elements(height_data) gt 0 then begin
	  f_c = n_elements(height_data[*,0])
		field_count = ((f_c eq 2) ? f_c : f_c - 1) /2
		gr_min = min(height_data[1:field_count,*], max=gr_max)
		tot_min = min([gr_min, gr_max, dem_min, dem_max], max=tot_max)
	endif else begin
		field_count = 0
		gr_min = dem_min
		gr_max = dem_max
		tot_min = dem_min
		tot_max = dem_max
	endelse

	; calculate the altitude range
	new_min = gr_min - 200	; in meters
	new_max = gr_max + 100	; in meters

	; calculate if the matrix fits in the draw area
	; if not crop it.
	dms = size(matrix)
	sx = dms[1]
	sy = dms[2]
	dem_min_ndx = round(sy * (new_min - dem_min) / (dem_max - dem_min))
	dem_max_ndx = round(sy * (new_max - dem_min) / (dem_max - dem_min))

	if dem_min_ndx gt 0 or dem_max_ndx lt sy then begin
		; crop required at minimum alt and/or max alt of dem
		bi = max([0, dem_min_ndx])
		cbi = sy - bi
		ti = min([sy - 1, dem_max_ndx])
		cti = sy - ti
		mtx = matrix[*, cti:cbi]
	endif else begin
		mtx = matrix
	endelse

	; stretch the matrix in altitude direction if needed
	dem_min = max([new_min, dem_min])
	dem_max = min([new_max, dem_max])
	dms = size(mtx)
	sx = dms[1]
	sy = dms[2]
	draw_dem_height = draw_height * (dem_max - dem_min) / (new_max - new_min)
	dem_y_offset = draw_height * (dem_min - new_min) / (new_max - new_min)
	mtx = congrid(mtx, draw_width + 1, draw_dem_height + 1)

	; Start the actual drawing:
    erase, 255	; white

	; scale matrix values to [0..251]
	mn = min(mtx, max = mx)
	ots = !d.table_size
	mtx = (ots - 4) * (mtx - mn) / (mx - mn)

	; draw the stratification
	tv, mtx, margin_left, margin_bottom, /order, /normal

	; plot the axis (to define the ranges)
	plot, freq $
		, background = 255 $ ; white
		, color = 0 $	; black
		, /nodata $
		, /noerase $
		, xrange = [1, 36] $
		, ticklen = -0.02 $
		, xticks = 18 $
		, xtickv = [1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36] $
		, yrange = [new_min, new_max], /ystyle $
;		, ytickinterval = 250 $
;		, ymargin = [0, 0] $
	  	, Position = [margin_left, margin_bottom $
	  					, margin_left + margin_width $
	  					, margin_bottom + margin_height] $
	  	, font = 1 $
	  	, charsize = character_size $
	  	, charthick = character_thick


	; draw title of x-axis separately (default is too close to x-axis)
	xtx_pos = margin_left + margin_width / 2
	xty_pos = margin_bottom / 6

	xyouts, xtx_pos, xty_pos, 'Time [10 day interval]' $
		, alignment = 0.5 $
		, font = 1 $
		, charsize = character_size $
	  	, charthick = character_thick $
		, color = 0 $
		, /normal

	axis, margin_left, margin_bottom + margin_height $
		, xticks = 6 $
		, xtickv = [1,6,12,18,24,30,36] $
		, color = 0 $	; black
	  	, font = 1 $
	  	, charsize = character_size $
	  	, charthick = character_thick $
	  	, /normal $
		, ticklen = -0.02 $
		, /xaxis

	; draw title of y-axis separately (font 0 does not handle orientation properly)
	ytx_pos = margin_left / 4
	yty_pos = margin_bottom + margin_height / 2

	xyouts, ytx_pos, yty_pos, 'Altitude [m]' $
		, orientation = 90 $
		, alignment = 0.5 $
		, font = 1 $
		, charsize = character_size $
	  	, charthick = character_thick $
		, color = 0 $
		, /normal

	; draw the altitude graphs for the different species
	field_color = 254
	_sym = -5	; 4 = diamond, 5 = triangle, 6 = square; negative: connect with lines
	if field_count gt 0 then begin
		for tc = 0, field_count - 1 do begin
			x_as = height_data[0, *]
			y_as = height_data[tc + 1, *]
			line_color = field_color - tc
			draw_symbol, _sym, line_thick
			oplot, x_as, y_as $
				, color = line_color $
				, psym = -8 $		; draw the symbol user-defined (in draw_symbol)
				, symsize = symbol_size $
				, linestyle = 0 $
				, thick = line_thick
			_sym -= 1
		endfor
	endif

	; draw the labels
	field_color = 254
	if field_count gt 0 then begin
		for tc = 0, field_count - 1 do begin
			x_off = -0.015
			y_off = 0.025
			if tc eq 0 then begin
				x_off *= -1.0
				y_off *= -1.0
			endif
			line_color = field_color - tc
			x_as = height_data[0, *]
			y_as = height_data[tc + 1, *]
			labels = height_data[(tc + 1) + field_count, *]
			for index = 0, n_elements(x_as) - 1 do begin
				factor = 1.0
				if index gt 19 then begin	; past middle of summer
					factor = -1.0
				endif
				if (factor * x_off) lt 0 then factor *= 1.2
				x = x_as[index]
				y = y_as[index]
				label = labels[index]
				if finite(label) eq 1 then begin
					x = margin_left + (1.0 * (x - 1) / 35) * margin_width
					y = margin_bottom + ((y - new_min) / (new_max - new_min)) * margin_height
					str = string(round(label), format = '(i-0)')
					xyouts, x + x_off * factor, y + y_off, str $
						, font = 1 $
						, charsize = character_size * 0.9 $
					  	, charthick = character_thick $
						, color = line_color $
						, /normal
				endif
			end
		endfor
	endif

	; plot the legend for the species graph(s)
	leg_steps = 10
    base_y = margin_bottom + margin_height / 3
    top_y = margin_bottom + margin_height
    base_y_px = margin_bottom * plot_height + draw_height / 3
    top_y_px = margin_bottom * plot_height + draw_height
    step_y = (top_y - base_y) / leg_steps

	left_leg = margin_left + 0.01
	width_leg = 0.04

	line_size = 0.06
	line_base = 0.019
	; draw the legend box
	if field_count gt 0 then begin
		bg_y = margin_bottom + margin_height - 1.0/50
		tg_y = margin_bottom + margin_height - 1.0/50 - field_count * line_size
		width_g = 0.2
		polyfill,[ [left_leg, bg_y] $
	    		, [left_leg, tg_y] $
	    		, [left_leg + width_g, tg_y] $
	    		, [left_leg + width_g, bg_y] $
	    		, [left_leg, bg_y] ] $
	    		, /normal $
	    		, color = 255
	    plots, left_leg, bg_y, /normal
	    plots,  [ [left_leg, bg_y] $
	    		, [left_leg, tg_y] $
	    		, [left_leg + width_g, tg_y] $
	    		, [left_leg + width_g, bg_y] $
	    		, [left_leg, bg_y] ] $
	    		, /normal $
	    		, color = 0 $
	    		, /continue

		; draw the legend for altitude graphs for the different species
		field_color = 254
		_sym = -5	; 4 = diamond, 5 = triangle, 6 = square; negative: connect with lines
		sndx = strsplit(header, ',', count = count)
		for tc = 0, field_count - 1 do begin
			line_color = field_color - tc
			aiai = [ [left_leg + 0.01, bg_y - (tc + 1) * line_size + line_base] $
					,[left_leg + 0.03, bg_y - (tc + 1) * line_size + line_base + 0.02] $
					,[left_leg + 0.05, bg_y - (tc + 1) * line_size + line_base]]
			plots, left_leg + 0.01, bg_y - (tc + 1) * line_size + line_base, /normal
			draw_symbol, _sym, line_thick
			plots, aiai $
				, color = line_color $
				, psym = -8 $
				, symsize = symbol_size $
				, linestyle = 0 $
				, thick = line_thick $
				, /normal $
				, /continue
			_sym -= 1
			if tc lt n_elements(height_data) - 1 then begin
				label = strmid(header, sndx[tc + 1], sndx[tc + 2] - sndx[tc + 1] - 1)
			endif else $
				label = strmid(header, sndx[tc + 1])
			xyouts, left_leg + 0.065, bg_y - (tc + 1) * line_size + line_base, label $
				, /normal $
	    		, charsize = character_size * 0.9 $
			  	, charthick = character_thick $
				, font=1 $
				, color = 0
		endfor
	endif

	; draw the legend bar
    leg_steps = 10
    base_y = margin_bottom
    top_y = margin_bottom + margin_height
    base_y_px = margin_bottom * plot_height
    top_y_px = margin_bottom * plot_height + draw_height
    step_y = (top_y - base_y) / leg_steps

	left_leg = 0.91
	width_leg = 6.0/15
    leg_data = findgen(1, 251)
    legend = congrid(leg_data, width_leg * legend_width, top_y_px - base_y_px + 1)
    tv, legend, left_leg, base_y, /normal

	width_leg *= (5.0 /60)

	; plot the RPD legend
	y = top_y + 0.025
	xyouts, left_leg, y, 'RPD(%)' $
		, font = 1 $
   		, charsize = character_size $
	  	, charthick = character_thick $
		, color = 0 $
		, /normal
    plots, left_leg, base_y, /normal
    plots,  [ [left_leg, base_y] $
    		, [left_leg, top_y] $
    		, [left_leg + width_leg, top_y] $
    		, [left_leg + width_leg, base_y] $
    		, [left_leg, base_y] ] $
    		, color = 0 $
    		, /normal $
    		, /continue
    for y = base_y, top_y, step_y do begin
    	plots, left_leg, y, color = 0, /normal
    	plots, [[left_leg, y], [left_leg + width_leg, y]], color = 0, /normal
    endfor
    hv = 0.0
    hv_s = 100.0 / (leg_steps / 2)
    for ii = 0, leg_steps, 2 do begin
    	y = base_y + ii * step_y
    	if ii eq leg_steps then y -= 0.02
    	str = string(hv, format='(i3)')
    	xyouts, left_leg + width_leg + 0.0016, y, str $
    		, charsize = character_size $
		  	, charthick = character_thick $
    		, font = 1 $
    		, color = 0 $
    		, /normal
    	hv += hv_s
    endfor

	; reset color table
;	loadct, 0, /silent	; b/w color lut (default)
end

pro handle_strat_Calculate, event
	widget_control, event.top, get_uvalue=state
	widget_control, state.stratoutput, get_value=filename
	widget_control, state.HeightStep, get_value=HeightStepString
	widget_control, state.stratoutput, get_value = outputfile
	widget_control, state.heightTable, get_value=height_table

	height_step = 100

	if strlen(HeightStepString) gt 0 then begin
		on_ioerror, bad_height_step
		height_step = fix(HeightStepString)
		height_step = height_step[0]

		; reset error_handler
		on_ioerror, NULL
	endif

	field_count = 0
	if strlen(height_table) gt 0 then begin
		ht_data = READ_ASCII(height_table $
			, HEADER = header $
			, DELIMITER = ',' $
			, DATA_START = 1 $
			, RECORD_START = 0, COUNT = count)

		field_count = n_elements(ht_data.field1) / count
		height_data = ht_data.field1
	endif

	fid = state.inputID
	dem_id = state.inputDEMID

	strat_read, fid, images, dims, is_valid
	if is_valid eq 0 then begin
		ans = Dialog_Message('Data type of timeseries is not float/double', /information, title='Stratification warning')
		return
	end

	; initialise tranquilizer
	envi_report_init,["Performing stratification",$
                      "This can take a few minutes"],$
						base = tranq, title = "progress"

	strat_read_DEM, dem_id, dem, dims

	strat_stratify, images, dem, height_step, matrix, tranq

	envi_report_init, base=tranq, /finish

	if strlen(outputfile) gt 0 then $
		envi_write_envi_file, matrix, out_name=outputfile

	state.matrix = ptr_new(matrix, /no_copy)
  if strlen(height_table) gt 0 then begin
    state.height_data = ptr_new(height_data, /no_copy)
    state.header = header
  endif

	dem_min = min(dem, max=dem_max)
	state.dem_min = dem_min
	state.dem_max = dem_max

	widget_control, event.top, set_uvalue=state

	; now display everything
	handleChangeLutType, event


	goto, done

bad_height_step:
	ans = Dialog_Message('Number required for height step', /information, title='Stratification warning')
	goto, done

done:
end

pro get_bw_lut, lut
	lut = $
		[[0,0,0], $
		[0,0,0], $
		[1,1,1], $
		[1,1,1], $
		[2,2,2], $
		[2,2,2], $
		[3,3,3], $
		[3,3,3], $
		[4,4,4], $
		[4,4,4], $
		[4,4,4], $
		[5,5,5], $
		[5,5,5], $
		[6,6,6], $
		[6,6,6], $
		[7,7,7], $
		[7,7,7], $
		[8,8,8], $
		[8,8,8], $
		[8,8,8], $
		[9,9,9], $
		[9,9,9], $
		[10,10,10], $
		[10,10,10], $
		[11,11,11], $
		[11,11,11], $
		[12,12,12], $
		[12,12,12], $
		[13,13,13], $
		[13,13,13], $
		[13,13,13], $
		[14,14,14], $
		[18,18,18], $
		[22,22,22], $
		[27,27,27], $
		[30,30,30], $
		[34,34,34], $
		[39,39,39], $
		[43,43,43], $
		[47,47,47], $
		[52,52,52], $
		[56,56,56], $
		[60,60,60], $
		[64,64,64], $
		[68,68,68], $
		[72,72,72], $
		[77,77,77], $
		[81,81,81], $
		[85,85,85], $
		[89,89,89], $
		[93,93,93], $
		[97,97,97], $
		[102,102,102], $
		[106,106,106], $
		[110,110,110], $
		[114,114,114], $
		[118,118,118], $
		[122,122,122], $
		[127,127,127], $
		[131,131,131], $
		[135,135,135], $
		[139,139,139], $
		[143,143,143], $
		[148,148,148], $
		[155,155,155], $
		[158,158,158], $
		[161,161,161], $
		[166,166,166], $
		[171,171,171], $
		[176,176,176], $
		[179,179,179], $
		[177,177,177], $
		[177,177,177], $
		[175,175,175], $
		[175,175,175], $
		[174,174,174], $
		[173,173,173], $
		[172,172,172], $
		[171,171,171], $
		[171,171,171], $
		[170,170,170], $
		[169,169,169], $
		[168,168,168], $
		[167,167,167], $
		[167,167,167], $
		[165,165,165], $
		[165,165,165], $
		[164,164,164], $
		[163,163,163], $
		[162,162,162], $
		[162,162,162], $
		[160,160,160], $
		[160,160,160], $
		[159,159,159], $
		[158,158,158], $
		[157,157,157], $
		[157,157,157], $
		[156,156,156], $
		[155,155,155], $
		[154,154,154], $
		[153,153,153], $
		[152,152,152], $
		[152,152,152], $
		[150,150,150], $
		[152,152,152], $
		[154,154,154], $
		[157,157,157], $
		[159,159,159], $
		[160,160,160], $
		[164,164,164], $
		[166,166,166], $
		[167,167,167], $
		[168,168,168], $
		[169,169,169], $
		[171,171,171], $
		[172,172,172], $
		[173,173,173], $
		[174,174,174], $
		[176,176,176], $
		[177,177,177], $
		[178,178,178], $
		[179,179,179], $
		[180,180,180], $
		[182,182,182], $
		[183,183,183], $
		[184,184,184], $
		[186,186,186], $
		[187,187,187], $
		[188,188,188], $
		[189,189,189], $
		[191,191,191], $
		[192,192,192], $
		[193,193,193], $
		[194,194,194], $
		[195,195,195], $
		[197,197,197], $
		[198,198,198], $
		[199,199,199], $
		[201,201,201], $
		[202,202,202], $
		[203,203,203], $
		[204,204,204], $
		[206,206,206], $
		[207,207,207], $
		[208,208,208], $
		[209,209,209], $
		[210,210,210], $
		[212,212,212], $
		[213,213,213], $
		[214,214,214], $
		[216,216,216], $
		[217,217,217], $
		[218,218,218], $
		[219,219,219], $
		[221,221,221], $
		[222,222,222], $
		[223,223,223], $
		[224,224,224], $
		[225,225,225], $
		[227,227,227], $
		[226,226,226], $
		[226,226,226], $
		[225,225,225], $
		[225,225,225], $
		[225,225,225], $
		[224,224,224], $
		[224,224,224], $
		[223,223,223], $
		[223,223,223], $
		[222,222,222], $
		[222,222,222], $
		[222,222,222], $
		[221,221,221], $
		[221,221,221], $
		[220,220,220], $
		[220,220,220], $
		[220,220,220], $
		[219,219,219], $
		[219,219,219], $
		[218,218,218], $
		[218,218,218], $
		[218,218,218], $
		[217,217,217], $
		[217,217,217], $
		[216,216,216], $
		[216,216,216], $
		[215,215,215], $
		[215,215,215], $
		[215,215,215], $
		[214,214,214], $
		[214,214,214], $
		[213,213,213], $
		[213,213,213], $
		[213,213,213], $
		[212,212,212], $
		[212,212,212], $
		[211,211,211], $
		[211,211,211], $
		[210,210,210], $
		[210,210,210], $
		[210,210,210], $
		[209,209,209], $
		[209,209,209], $
		[208,208,208], $
		[208,208,208], $
		[207,207,207], $
		[207,207,207], $
		[207,207,207], $
		[206,206,206], $
		[206,206,206], $
		[206,206,206], $
		[205,205,205], $
		[205,205,205], $
		[204,204,204], $
		[204,204,204], $
		[203,203,203], $
		[203,203,203], $
		[203,203,203], $
		[202,202,202], $
		[202,202,202], $
		[201,201,201], $
		[201,201,201], $
		[200,200,200], $
		[200,200,200], $
		[200,200,200], $
		[199,199,199], $
		[199,199,199], $
		[199,199,199], $
		[194,194,194], $
		[189,189,189], $
		[184,184,184], $
		[180,180,180], $
		[176,176,176], $
		[171,171,171], $
		[167,167,167], $
		[162,162,162], $
		[157,157,157], $
		[153,153,153], $
		[148,148,148], $
		[144,144,144], $
		[140,140,140], $
		[135,135,135], $
		[130,130,130], $
		[126,126,126], $
		[121,121,121], $
		[117,117,117], $
		[112,112,112], $
		[108,108,108], $
		[103,103,103], $
		[99,99,99], $
		[94,94,94], $
		[89,89,89], $
		[85,85,85], $
		; special colors:
		[128,128,128], $	; gray
		[160,160,160], $	; light gray
		[255,255,255]]		; white
end

pro get_yellow_rainbow_lut, lut
	lut = $
		[[0,0,0], $
		[0,0,4], $
		[0,0,8], $
		[0,0,12], $
		[0,0,16], $
		[0,0,20], $
		[0,0,24], $
		[0,0,28], $
		[0,0,32], $
		[0,0,36], $
		[0,0,40], $
		[0,0,45], $
		[0,0,49], $
		[0,0,53], $
		[0,0,57], $
		[0,0,61], $
		[0,0,65], $
		[0,0,69], $
		[0,0,73], $
		[0,0,77], $
		[0,0,81], $
		[0,0,86], $
		[0,0,90], $
		[0,0,94], $
		[0,0,98], $
		[0,0,102], $
		[0,0,106], $
		[0,0,110], $
		[0,0,114], $
		[0,0,118], $
		[0,0,122], $
		[0,0,127], $
		[0,6,131], $
		[0,12,135], $
		[0,19,139], $
		[0,25,143], $
		[0,31,147], $
		[0,38,151], $
		[0,44,155], $
		[0,50,159], $
		[0,57,163], $
		[0,63,167], $
		[0,69,171], $
		[0,76,175], $
		[0,82,179], $
		[0,88,183], $
		[0,95,187], $
		[0,101,191], $
		[0,107,195], $
		[0,114,199], $
		[0,120,203], $
		[0,126,207], $
		[0,133,211], $
		[0,139,215], $
		[0,145,219], $
		[0,152,223], $
		[0,158,227], $
		[0,164,231], $
		[0,171,235], $
		[0,177,239], $
		[0,183,243], $
		[0,190,247], $
		[0,196,251], $
		[0,203,255], $
		[0,216,255], $
		[0,220,255], $
		[0,225,255], $
		[0,233,255], $
		[0,242,255], $
		[0,250,255], $
		[0,255,255], $
		[0,255,242], $
		[0,255,238], $
		[0,255,225], $
		[0,255,220], $
		[0,255,216], $
		[0,255,203], $
		[0,255,199], $
		[0,255,191], $
		[0,255,187], $
		[0,255,174], $
		[0,255,170], $
		[0,255,157], $
		[0,255,152], $
		[0,255,148], $
		[0,255,135], $
		[0,255,131], $
		[0,255,123], $
		[0,255,114], $
		[0,255,106], $
		[0,255,102], $
		[0,255,89], $
		[0,255,84], $
		[0,255,80], $
		[0,255,67], $
		[0,255,63], $
		[0,255,55], $
		[0,255,46], $
		[0,255,38], $
		[0,255,34], $
		[0,255,21], $
		[0,255,16], $
		[0,255,12], $
		[0,255,0], $
		[4,255,0], $
		[12,255,0], $
		[21,255,0], $
		[29,255,0], $
		[33,255,0], $
		[46,255,0], $
		[51,255,0], $
		[55,255,0], $
		[59,255,0], $
		[63,255,0], $
		[67,255,0], $
		[71,255,0], $
		[75,255,0], $
		[80,255,0], $
		[84,255,0], $
		[88,255,0], $
		[92,255,0], $
		[96,255,0], $
		[100,255,0], $
		[105,255,0], $
		[109,255,0], $
		[113,255,0], $
		[117,255,0], $
		[121,255,0], $
		[125,255,0], $
		[130,255,0], $
		[134,255,0], $
		[138,255,0], $
		[142,255,0], $
		[146,255,0], $
		[150,255,0], $
		[155,255,0], $
		[159,255,0], $
		[163,255,0], $
		[167,255,0], $
		[171,255,0], $
		[175,255,0], $
		[180,255,0], $
		[184,255,0], $
		[188,255,0], $
		[192,255,0], $
		[196,255,0], $
		[200,255,0], $
		[205,255,0], $
		[209,255,0], $
		[213,255,0], $
		[217,255,0], $
		[221,255,0], $
		[225,255,0], $
		[230,255,0], $
		[234,255,0], $
		[238,255,0], $
		[242,255,0], $
		[246,255,0], $
		[250,255,0], $
		[255,255,0], $
		[255,254,0], $
		[255,253,0], $
		[255,252,0], $
		[255,252,0], $
		[255,251,0], $
		[255,250,0], $
		[255,250,0], $
		[255,249,0], $
		[255,248,0], $
		[255,247,0], $
		[255,247,0], $
		[255,246,0], $
		[255,245,0], $
		[255,245,0], $
		[255,244,0], $
		[255,243,0], $
		[255,243,0], $
		[255,242,0], $
		[255,241,0], $
		[255,240,0], $
		[255,240,0], $
		[255,239,0], $
		[255,238,0], $
		[255,238,0], $
		[255,237,0], $
		[255,236,0], $
		[255,235,0], $
		[255,235,0], $
		[255,234,0], $
		[255,233,0], $
		[255,233,0], $
		[255,232,0], $
		[255,231,0], $
		[255,231,0], $
		[255,230,0], $
		[255,229,0], $
		[255,228,0], $
		[255,228,0], $
		[255,227,0], $
		[255,226,0], $
		[255,226,0], $
		[255,225,0], $
		[255,224,0], $
		[255,223,0], $
		[255,223,0], $
		[255,222,0], $
		[255,221,0], $
		[255,221,0], $
		[255,220,0], $
		[255,219,0], $
		[255,219,0], $
		[255,218,0], $
		[255,217,0], $
		[255,216,0], $
		[255,216,0], $
		[255,215,0], $
		[255,214,0], $
		[255,214,0], $
		[255,213,0], $
		[255,212,0], $
		[255,211,0], $
		[255,211,0], $
		[255,210,0], $
		[255,209,0], $
		[255,209,0], $
		[255,208,0], $
		[255,207,0], $
		[255,207,0], $
		[255,199,0], $
		[255,191,0], $
		[255,183,0], $
		[255,176,0], $
		[255,168,0], $
		[255,160,0], $
		[255,153,0], $
		[255,145,0], $
		[255,137,0], $
		[255,130,0], $
		[255,122,0], $
		[255,114,0], $
		[255,107,0], $
		[255,99,0], $
		[255,91,0], $
		[255,84,0], $
		[255,76,0], $
		[255,68,0], $
		[255,61,0], $
		[255,53,0], $
		[255,45,0], $
		[255,38,0], $
		[255,30,0], $
		[255,22,0], $
		[255,15,0], $
		; special colors:
		[128,128,128], $	; gray
		[160,160,160], $	; light gray
		[255,255,255]]		; white
end

pro draw_symbol, num, thick
	; 5 = triangle
	; 6 = square
	if abs(num) eq 6 then begin
		x = [-0.5, -0.5, 0.5, 0.5]
		y = [-0.5, 0.5, 0.5, -0.5]
		usersym, x, y, /fill
	endif else if abs(num) eq 5 then begin
		x = [-0.5, -0.5, 0.5, 0.5, -0.5]
		y = [-0.5, 0.5, 0.5, -0.5, -0.5]
		usersym, x, y, thick = thick
;		x = [-0.5, 0.0,  0.9, -0.5]
;		y = [-0.5, 0.9, -0.5, -0.5]
	endif
end