pro nrs_stratify_ChangeLutType, event
  compile_opt idl2, logical_predicate

	widget_control, event.top, get_uvalue = state
	val_fld = widget_info(event.top, find_by_uname='nrs_stratify_draw')
	widget_control, val_fld, get_value = draw_pane
	
	val_fld = widget_info(event.top, find_by_uname='nrs_stratify_lutTypeCombobox')
	lut_selected = widget_info(val_fld, /combobox_gettext)

  colors = ['rainbow', 'black/white', 'custom']
  cix = where(strlowcase(lut_selected) eq colors)
  lut_id = ([39, 0, -1])[cix[0]]
  
  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_color_table')
  widget_control, val_fld, sensitiv = (lut_id eq -1)

  wset, draw_pane

  lut = []
  case lut_id of
    -1 : begin
           val_fld = widget_info(event.top, find_by_uname='nrs_stratify_color_table')
           widget_control, val_fld, get_value = color_table
           lut = nrs_get_custom_lut(color_table)
         end
     0 : lut = nrs_get_bw_lut()
    39 : lut = get_yellow_rainbow_lut()
	endcase
  if n_elements(lut) gt 0 then begin
    ots = !d.table_size
    nrcol = ots - state.reserved_colors
    lut2 = congrid(lut[*, 0:ots - 3], 3, nrcol)
    lut[*, 0:nrcol - 1]  = lut2
    tvlct, r, g, b, /get
    rc = [transpose(r[nrcol : ots - 1]), transpose(g[nrcol : ots - 1]), transpose(b[nrcol : ots - 1])]
    lut[*, nrcol: nrcol+n_elements(rc[0,*])-1] = rc
    tvlct, lut[0,*], lut[1,*], lut[2,*]
	  nrs_stratify_draw_graph, event
	endif
	
end

pro nrs_stratify_color_select, event
  compile_opt idl2
  
  widget_control, event.top, get_uvalue = state
  
  sel_col = cgPickColorName(index = 250, cancel = cancel)
  if cancel eq 1 then return
  
  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_draw_id')
  widget_control, val_fld, get_value = dispid
  cur_win = !d.window
  wset, dispid
  erase, color = 250
  wset, cur_win ; restore old window
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_track_method')
;  widget_control, val_fld, get_value = track_method ; 0 = replace, 1 = add
;  if track_method eq 1 then state.line_colix++ ; add track data
  
  widget_control, event.top, set_uvalue = state
end

pro nrs_stratify_set_window, event
  compile_opt idl2
  
  widget_control, event.top, get_uvalue = state
  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_smooth_window')
  widget_control, val_fld, get_value = win_str
  
  new_win = fix(win_str)
  if new_win eq state.smooth_win then return
   
  state.smooth_win = new_win

  widget_control, event.top, set_uvalue = state

  nrs_stratify_draw_graph, event
  
end

function nrs_get_reserved_colors
  rc = [[ 79, 129, 189], $
        [168,  66,  63], $
        [134, 164,  74], $
        [110,  84, 141], $
        [ 61, 150, 174], $
        [218, 129,  55], $
        [142, 165, 203], $
        [206, 142, 141], $
        [181, 202, 146], $
        [165, 151, 185]]

  return, rc
end
 
pro nrs_stratify_handle_input, event
  compile_opt idl2, logical_predicate

	val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_refstack')
  widget_control, val_fld, get_value = ref

  ref = strtrim(ref, 2)
  ext = nrs_get_file_extension(strlowcase(ref))
  ix = where(ext[0] eq ['.csv', '.txt'], cnt)
  
  if cnt eq 0 then begin
    envi_open_file, ref, r_fid = fid, /no_realize, /no_interactive_query
  	if fid eq -1 then return
  
  	envi_file_query, fid, nb = inputlayers
  	if inputlayers eq 1 then begin
  	  void = error_message('Timeseries needs more than one layer', /warning, title = 'Stratification warning')
  		return
  	endif
  endif else begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_list')
    lst = nrs_read_listfile(ref)
    
    crlf = string(13b) + string(10b)
    msg = 'Stacks in list file: ' + crlf + strjoin(string(lst, format = '("''",a,"''")'), crlf)
    widget_control, val_fld, set_value = msg
  endelse

end

function nrs_stratify_handle_track_toggles, event
  compile_opt idl2, logical_predicate

  widget_control, event.top, get_uvalue = state
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_track_toggles')
  widget_control, val_fld, get_value = toggles
  
  state.show_points = toggles[0]
  state.show_line = toggles[1]
  state.show_legend = toggles[2]
  state.smooth_line = toggles[3]
  widget_control, event.top, set_uvalue = state

  nrs_stratify_draw_graph, event

  return, event
end

pro nrs_stratify_display_track, event
  compile_opt idl2, logical_predicate

  widget_control, event.top, get_uvalue = state

  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_draw')
  widget_control, val_fld, get_value = draw_pane

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_track_method')
  widget_control, val_fld, get_value = track_method ; 0 = replace, 1 = add
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_track_toggles')
  widget_control, val_fld, get_value = toggles
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_table')
  widget_control, val_fld, get_value = table
  table = strtrim(table, 2)
  
  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_start_year')
  widget_control, val_fld, get_value = year_str
  start_year = 0
  if strlen(strtrim(year_str, 2)) gt 0 then start_year = fix(year_str)

  field_count = 0
  if strlen(table) eq 0 then return

  wset, draw_pane
  
  line_data = nrs_read_table(table, col_count = field_count, header = header, valid = valid)
  if valid eq 0 then return

  state.show_points = toggles[0]
  state.show_line = toggles[1]
  state.show_legend = toggles[2]
  state.smooth_line = toggles[3]
  x_min = state.x_min
  x_max = state.x_max
  sy = state.start_year
  ey = state.end_year
  nryears = ey - sy + 1
  nb = x_max - x_min + 1

  if sy ne start_year then begin
    sy = start_year
    ey = start_year + nryears - 1
    state.start_year = start_year
    state.end_year = start_year + nryears - 1
  endif

  jd = [julday(1, 1, sy), julday(12, 31, ey)]
  per = nrs_get_period_from_range(jd[0], jd[1], nb, per_str = per_str)
  nrs_get_dt_indices, jd, period = 'day', julian_out = jo
  nrs_get_dt_indices, jo, period = per_str, indices = ind, ri = ri, /clip

  nrdays = jd[1] - jd[0] + 1
  nrrec = n_elements(line_data[0, *])
  dt = long(reform(julday(1, 1, line_data[0, *]) + line_data[1, *] - 1, nrrec))  ; julian days from table
  list_match, dt, jo, index, rev_ind
;  bix = ri[rev_ind] + 1 ; data table positions in table for current data set
  bix = rev_ind
  
  if rev_ind[0] eq -1 then return
  
  newdat = fltarr(field_count - 2, nrdays)
  newdat[*, bix] = line_data[2 : -1, *]
  colnames = strsplit(header, ',;\t', /extract)
  
  if track_method eq 1 then begin ; add track data
    ld_ar = []
    fields = []
    if ptr_valid(state.line_data) then begin
      ld_ar = *(state.line_data)
      fields = strsplit(state.header, ',;\t', /extract)
      fns = strsplit(state.tracks, ',;\t', /extract)
    endif
    ld_ar = [ld_ar, newdat] ; append the new data

    state.line_data = ptr_new(ld_ar, /no_copy)
    state.header = strjoin([fields, colnames[2 : -1]], ',')
    state.tracks = strjoin([state.tracks, table], ',')
     
    state.line_colix++
    if state.line_colix ge 250 then state.line_colix = 239
  endif else begin
    if ptr_valid(state.line_data) then ptr_free, state.line_data
    state.line_data = ptr_new(newdat, /no_copy)
    state.header = strjoin(colnames[2 : -1], ',')
    state.tracks = table
    
    state.line_colix = 240
  endelse
  
  tvlct, r, g, b, /get
  track_color = 250 ; index of last selected color
  r[state.line_colix] = r[track_color]
  g[state.line_colix] = g[track_color]
  b[state.line_colix] = b[track_color]
  tvlct, r, g, b  ; assign the new color to the track color

  widget_control, event.top, set_uvalue = state

  nrs_stratify_draw_graph, event
end

pro nrs_stratify_sv_tables, event
  compile_opt idl2, logical_predicate

  widget_control, event.top, get_uvalue = state
  
  if ~ptr_valid(state.matrix) then return
  
  matrix = *(state.matrix)
  sz = size(matrix, /dim)
  
  tracks = strsplit(state.tracks, ',', count = count, /extract)
  if count le 0 then return
  
  x_min = state.x_min
  x_max = state.x_max
  y_min = state.dem_min
  y_max = state.dem_max

  sy = state.start_year
  ey = state.end_year
  nryears = ey - sy + 1
  nb = x_max - x_min + 1

  jd = [julday(1, 1, sy), julday(12, 31, ey)]
  per = nrs_get_period_from_range(jd[0], jd[1], nb, per_str = per_str)
  nrs_get_dt_indices, jd, period = 'day', julian_out = jo
  nrs_get_dt_indices, jo, period = per_str, indices = ind, ri = ri, /clip

  for fn = 0, n_elements(tracks) - 1 do begin
    table = tracks[fn]
    line_data = nrs_read_table(table, col_count = field_count, header = header, valid = valid)
    if valid eq 0 then continue

    nrrec = n_elements(line_data[0, *])
    dt = long(reform(julday(1, 1, line_data[0, *]) + line_data[1, *] - 1, nrrec))  ; julian days from table
    list_match, dt, jo, index, rev_ind
    ix = ri[rev_ind] + 1 ; data table positions in table for current data set
  
    if rev_ind[0] eq -1 then continue

    newdat = line_data[2 : -1, *]
    iy = round((sz[1] - 1) * (newdat - y_min) / (y_max - y_min))
    ixx = transpose(ix)
    ix = []
    obs_hdr = ''
    for i = 1, field_count - 2 do begin
      ix = [ix, ixx]
      obs_hdr += string(i, format = '(",val", i0)')
    endfor 
    obs_val = matrix[ix, iy]
    outdata = [line_data, obs_val]
    outheader = strsplit(header + obs_hdr, ',', /extract)
    
    tbl_out = getOutname(table, postfix = '_val', ext = '.csv')
    write_csv, tbl_out, outdata, header = outheader
  endfor
  
  ans = dialog_message('Finished saving tables')
end

pro nrs_stratify_SavePicture, event
  compile_opt idl2, logical_predicate

	widget_control, event.top, get_uvalue = state
	
	val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_output_JPG')
  widget_control, val_fld, get_value = filename
  filename = strtrim(filename, 2)
	
	if strlen(filename) eq 0 then return

	thisDrawing = !D.Name
	; remember current settings
	scale = state.draw_scale
	draw_width = state.draw_width

  sy = state.start_year
  ey = state.end_year
  ny = ey - sy + 1
  
  filename = getOutname(filename, postfix = '', ext = '.jpg')
	set_plot, 'Z'
	state.draw_scale = 5
	state.draw_width = draw_width * ny
	widget_control, event.top, set_uvalue = state
	Device, Set_Resolution = [ny * 3000, 1500], Z_Buffer = 0
      
	nrs_stratify_draw_graph, event

	img = tvrd()
	tvlct, r, g, b, /get
	device, z_buffer = 1

	set_plot, thisDrawing

	image24 = BytArr(3, ny * 3000, 1500)
 	image24[0,*,*] = r[img]
 	image24[1,*,*] = g[img]
 	image24[2,*,*] = b[img]

	write_jpeg, filename, image24, true = 1, quality = 75  ; true = 1: BIP

  ; restore current settings
	state.draw_scale = scale
	state.draw_width = draw_width
	widget_control, event.top, set_uvalue = state
end

pro nrs_stratify_save_strat, event
  compile_opt idl2, logical_predicate

  widget_control, event.top, get_uvalue = state
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_output_image')
  widget_control, val_fld, get_value = filename
  filename = strtrim(filename, 2)
  
  if strlen(filename) eq 0 then return

  if ~ptr_valid(state.matrix) then return

  matrix = *(state.matrix)
  
  envi_write_envi_file, matrix, out_name = filename, /no_realize, /no_open
end

pro nrs_stratify_load_strat, event
  compile_opt idl2, logical_predicate, hidden

  widget_control, event.top, get_uvalue = state
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_output_image')
  widget_control, val_fld, get_value = filename
  filename = strtrim(filename, 2)
  
  if strlen(filename) eq 0 then return
  
  envi_open_file, filename, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Unable to load stratification', /error)
    return
  endif
  
  envi_file_query, fid, dims = dims, nl = steps
  matrix = envi_get_data(fid = fid, dims = dims, pos = [0])

  state.matrix = ptr_new(matrix, /no_copy)
  state.mat_steps = steps
  state.fname = ''

  widget_control, event.top, set_uvalue = state
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_start_year')
  widget_control, val_fld, get_value = filename
  
  nrs_stratify_draw_graph, event
end

function nrs_stratify_ticks, axis, index, value, level
  compile_opt idl2
  
  month_abbr = 'jfmamjjasond'
  caldat, value, m  
  case level of
    0 : begin
          return, strmid(month_abbr, m - 1, 1)
        end
  endcase
end

pro nrs_stratify_draw_graph, event
  compile_opt idl2, logical_predicate

  widget_control, event.top, get_uvalue = state

  if ~ptr_valid(state.matrix) then return

  font_size =  !D.name eq 'Z' ? 1.7 : 1

  sy = state.start_year
  ey = state.end_year
  nryears = ey - sy + 1
  
  matrix = *(state.matrix)
  line_data = []
  header = ''
  if ptr_valid(state.line_data) then begin
    line_data = *(state.line_data)
    header = state.header
  end
  dem_min = state.dem_min
  dem_max = state.dem_max

  plot_height = state.draw_height * state.draw_scale
  plot_width = state.draw_width * state.draw_scale  ; pixels
  legend_height = state.draw_height * state.draw_scale
  legend_width = state.legend_width * state.draw_scale  ; pixels

  character_size = 0.9 * state.draw_scale
  character_thick = 1.0 * state.draw_scale
  line_thick = 2 * 1.5 * state.draw_scale - 0.75
  symbol_size = state.draw_scale

  margin_left = 0.10
  margin_bottom = 0.20
  margin_height = 0.70
  margin_width = 0.77
  draw_height = margin_height * plot_height - 1
  draw_width = margin_width * plot_width -1
  freq = indgen(23)

 if n_elements(line_data) gt 0 then begin
   field_count = n_elements(line_data[*, 0])
   gr_min = min(line_data[0 : field_count - 1,*], max = gr_max)
   tot_min = min([dem_min, dem_max], max = tot_max)
   gr_min = tot_min
   gr_max = tot_max
 endif else begin
   field_count = 0
   gr_min = dem_min
   gr_max = dem_max
   tot_min = dem_min
   tot_max = dem_max
 endelse

  ; calculate the latitude range
  new_min = gr_min
  new_max = gr_max

  ; calculate if the matrix fits in the draw area
  ; if not crop it.
  dms = size(matrix)
  sx = dms[1]
  sy = dms[2]
  dem_min_ndx = round(sy * (new_min - dem_min) / (dem_max - dem_min))
  dem_max_ndx = round(sy * (new_max - dem_min) / (dem_max - dem_min))

  mtx = matrix

  ; stretch the matrix in altitude direction if needed
  draw_dem_height = draw_height * (dem_max - dem_min) / (new_max - new_min)
  dem_y_offset = draw_height * (dem_min - new_min) / (new_max - new_min)
  mtx = congrid(mtx, draw_width + 1, draw_dem_height + 1)

  ; Start the actual drawing:
  device, decomposed = 0  ; we are using LUT colors
  erase, 255  ; white

  reserved_colors = state.reserved_colors
  ; scale matrix values to [0..(255 - reserved_colors)]
  mn = min(mtx, max = mx)
  ots = !d.table_size
  mtx = (ots - reserved_colors - 1) * (mtx - mn) / (mx - mn)

  ; draw the stratification
  tv, mtx, margin_left, margin_bottom, /order, /normal

  x_min = state.x_min
  x_max = state.x_max
  sy = state.start_year
  ey = state.end_year
  nryears = ey - sy + 1
  nrdays = julday(1, 1, ey + 1) - julday(1, 1, sy)
  nb = (x_max - x_min) / nryears
  month_start = julday(indgen(nryears * 12) mod 12 + 1, 15, indgen(nryears * 12) / 12 + sy)
  xticks = month_start - julday(1, 1, sy) ; x-axis is day based

  dummy = label_date(date_format='%m')
  ; plot the bottom X-axis (to define the ranges)
  plot, freq $
    , background = 255 $ ; white
    , color = 0 $ ; black
    , /nodata $
    , /noerase $
    , xrange = [x_min, x_max] + 1 $
    , xticklen = -0.02 $
    , xminor = 1 $
    , xtickv = xticks $
    , xticks = n_elements(xticks) - 1 $
    , xtickunit = 'Month' $
    , xtickformat = 'nrs_stratify_ticks' $
    , xstyle = 8 $
    , yticklen = -0.02 $
    , yrange = [new_min, new_max], /ystyle $
    , Position = [margin_left, margin_bottom $
                , margin_left + margin_width $
                , margin_bottom + margin_height] $
    , font = 1 $
    , charsize = character_size * font_size $
    , charthick = character_thick

  ; draw x-axis title separately (default: title is too close to x-axis)
  xtx_pos = margin_left + margin_width / 2
  xty_pos = margin_bottom / 10

  xyouts, xtx_pos, xty_pos, 'Date (month)' $
    , alignment = 0.5 $
    , font = 1 $
    , charsize = character_size * font_size $
    , charthick = character_thick $
    , color = 0 $
    , /normal

  ; Top X-axis
  ticks = indgen(nryears) * nb + nb / 2
  xtickname = string(indgen(ey - sy + 1) + sy, format = '(i0)')
  if nryears eq 1 then begin
    ticks = [1, ticks, nb + 1]
    xtickname = replicate(' ', 3)
    xtickname[1] = string(indgen(ey - sy + 1) + sy, format = '(i0)')
  endif
  axis, margin_left, margin_bottom + margin_height $
    , xrange = [x_min, x_max] + 1 $
    , xticks = n_elements(ticks) - 1 $
    , xtickv = ticks $ 
    , xtickname = xtickname $
    , xminor = 1 $
    , color = 0 $ ; black
    , font = 1 $
    , charsize = character_size * font_size $
    , charthick = character_thick $
    , /normal $
    , ticklen = -0.04 $
    , /xaxis

  ; draw title of y-axis separately (font 0 does not handle orientation properly)
  ytx_pos = margin_left / 4
  yty_pos = margin_bottom + margin_height / 2

  xyouts, ytx_pos, yty_pos, 'Latitude (degree)' $
    , orientation = 90 $
    , alignment = 0.5 $
    , font = 1 $
    , charsize = character_size * font_size $
    , charthick = character_thick $
    , color = 0 $
    , /normal

  ; draw the tracking graphs for the different species
;  field_color = ots - state.reserved_colors
  field_color = 240
  _sym = 0 ; 4 = diamond, 5 = square, 6 = up_triangle, 7 = circle
  field_count = 0
  if n_elements(line_data) gt 0 then $
    field_count = n_elements(line_data[*, 0])
  if field_count gt 0 then begin
    for tc = 0, field_count - 1 do begin
      y_as = line_data[tc, *]
      x_as = where(y_as ne 0, x_cnt)
      if x_cnt eq 0 then continue
      
      y_as = y_as[x_as]
      x_as_obs = x_as
      y_as_obs = y_as
      if state.smooth_line then begin
        steps = x_as[-1] - x_as[0] + 1
        x = indgen(steps) + x_as[0]
        n = interpol(y_as, x_as, x)
        x_as = indgen(n_elements(n)) + x_as[0]
        y_as = smooth(n, state.smooth_win)
      endif 
      
      line_color = field_color + tc
      if state.show_points then begin
        as_line = state.show_line eq 1 ? -1 : 1
        draw_symbol, _sym, line_thick
        oplot, x_as_obs, y_as_obs $
          , color = line_color $
          , psym = 8 $
          , symsize = symbol_size $
          , linestyle = 0 $
          , thick = line_thick
        _sym -= 1
      endif
      if state.show_line then $
        oplot, x_as, y_as $
          , color = line_color $
          , linestyle = 0 $
          , thick = line_thick
      
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
  if  state.show_legend && (field_count gt 0) then begin
    bg_y = margin_bottom + margin_height - 1.0/50
    tg_y = margin_bottom + margin_height - 1.0/50 - field_count * line_size
    width_g = 0.2 / nryears
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
;    field_color = 240
    _sym = 0 ; 4 = diamond, 5 = square, 6 = up_triangle, 7 = circle
    sndx = strsplit(header, ',', count = count, /extract)
    for tc = 0, field_count - 1 do begin
      line_color = field_color + tc
      aiai = [ [left_leg + 0.01, bg_y - (tc + 1) * line_size + line_base] $
          ,[left_leg + 0.03, bg_y - (tc + 1) * line_size + line_base + 0.02] $
          ,[left_leg + 0.05, bg_y - (tc + 1) * line_size + line_base]]
      plots, left_leg + 0.01, bg_y - (tc + 1) * line_size + line_base, /normal
      if state.show_points then begin
        draw_symbol, _sym, line_thick
        as_line = state.show_line eq 1 ? -1 : 1
        plots, aiai $
          , color = line_color $
          , psym = as_line * 8 $
          , symsize = symbol_size $
          , linestyle = 0 $
          , thick = line_thick $
          , /normal $
          , /continue
        _sym += 1
      endif else begin
        if state.show_line then $
          plots, aiai $
            , color = line_color $
            , linestyle = 0 $
            , thick = line_thick $
            , /normal $
            , /continue
      endelse
   
      label = sndx[tc]
      xyouts, left_leg + 0.065, bg_y - (tc + 1) * line_size + line_base, label $
        , /normal $
          , charsize =  * font_size * 0.9 $
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
    leg_data = findgen(1, ots - reserved_colors - 1)
    legend = congrid(leg_data, width_leg * legend_width, top_y_px - base_y_px + 1)
    tv, legend, left_leg, base_y, /normal

  width_leg *= (5.0 /60) / (!D.name eq 'Z' ? nryears : 1)

  ; plot the RPD legend
  y = top_y + 0.04;0.025
  xyouts, left_leg, y, 'RPD(%)' $
    , font = 1 $
    , charsize = character_size * font_size $
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
      , charsize = character_size * font_size $
      , charthick = character_thick $
      , font = 1 $
      , color = 0 $
      , /normal
    hv += hv_s
  endfor

  ; reset color table
; loadct, 0, /silent  ; b/w color lut (default)
end

pro draw_graph, event ; dem_min, dem_max, matrix, height_data, header: now in state struct
  compile_opt idl2, logical_predicate

	strat_draw = widget_info(event.top, find_by_uname='nrs_stratify_draw')
	combo_id = widget_info(event.top, find_by_uname='nrs_stratify_lutTypeCombobox')

	widget_control, event.top, get_uvalue = state

	if ~ptr_valid(state.matrix) then return

	matrix = *(state.matrix)
	height_data = []
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

pro nrs_stratify_calculate, event
  compile_opt idl2, logical_predicate
  
	widget_control, event.top, get_uvalue = state
	
	val_fld = widget_info(event.top, find_by_uname = 'nrs_stratify_refstack')
  widget_control, val_fld, get_value = img_list
  
  img_list = strtrim(img_list, 2)
  ext = nrs_get_file_extension(strlowcase(img_list))
  ix = where(ext[0] eq ['.csv', '.txt'], cnt)
  
  if cnt eq 0 then files = [img_list] $
  else files = nrs_read_listfile(img_list)

  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_latitude_step')
  widget_control, val_fld, get_value = steps_str
  steps_str = strtrim(steps_str, 2)
  steps = fix(steps_str[0])
  val_fld = widget_info(event.top, find_by_uname='nrs_stratify_start_year')
  widget_control, val_fld, get_value = year_str
  start_year = 0
  if strlen(strtrim(year_str, 2)) gt 0 then start_year = fix(year_str)

  ; (re)calculate matrix if data file has changed or number of steps have changed
  if ~ptr_valid(state.matrix) || state.fname ne img_list $
       || state.mat_steps ne steps then begin

    prog_outer = !NULL
    fid = -1
    nb_tot = 0
    
    ; outer loop progress indicator, if necessary
    if n_elements(files) gt 1 then $ 
      prog_outer = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                          , ysize = 15, title = "Stratification (Multiple)" $
                          , /fast_loop $
                          )
      nrs_set_progress_property, prog_outer, title = 'Calculate stratification series', /start

    vlist = []
    ; inner loop progress indicator
    prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                          , ysize = 15, title = "Stratification" $
                          , level = 1 $
                          , /fast_loop $
                          )
    
    full_mat = []
    i = 0
    tot = n_elements(files)
    state.start_year = start_year
    state.end_year = start_year + tot - 1
    foreach ref, files do begin
      if nrs_update_progress(prog_outer, i, tot, cancelled = cancelled) then begin
        ; remove inner loop progress indicator
        if prog_obj ne !null then $
          prog_obj -> Destroy
        
        return
      endif
      i++
      
      envi_open_file, ref, r_fid = fid, /no_realize, /no_interactive_query
      if fid eq -1 then begin
        print, 'Could not open ' + ref + '; skipped'
        continue  ; skip files that can not be opened
      endif else begin
        vlist = [vlist, ref]
        print, 'Stratifying ' + ref
      endelse
    
      envi_file_query, fid, nb = nb, nl = nl
      
  	  nrs_stratify, ref, steps, matrix = matrix, cancelled = cancelled, prog_obj = prog_obj
  	  
      if cancelled eq 1 then return
      
      full_mat = [full_mat, matrix]
      nb_tot += nb
    endforeach
  
    envi_convert_file_coordinates, fid, [0, 0], [0, nl - 1], crd_x, crd_y, /to_map
    mi = envi_get_map_info(fid = fid, undef = undef)
    if undef eq 1 || mi.proj.type eq 0 then begin
      state.dem_min = min(crd_y)
      state.dem_max = max(crd_y)
    endif else begin
      geo = envi_proj_create(/geographic) 
      envi_convert_projection_coordinates, crd_x, crd_y, mi.proj, lon, lat, geo
      state.dem_min = min(lat)
      state.dem_max = max(lat)
    endelse
    state.x_min = 0
    state.x_max = nb_tot - 1

    state.matrix = ptr_new(full_mat, /no_copy)
    state.mat_steps = steps
    state.fname = img_list
    
    ; remove inner loop progress indicator
    if prog_obj ne !null then $
      prog_obj -> Destroy
    
    ; remove outer loop progress indicator
    if prog_outer ne !null then $
      prog_outer -> Destroy
    
  endif
    
	widget_control, event.top, set_uvalue = state

	; now display everything
	nrs_stratify_ChangeLutType, event

end

; moved to nrs_utils
;function nrs_read_table, table, col_count = field_count, header = header, valid = valid 
;  compile_opt idl2, logical_predicate
;  
;  valid = 0
;  if strlen(table) eq 0 then return, []
;  
;  valid = 1
;  ht_data = read_ascii(table $
;    , header = header $
;    , delimiter = ',' $
;    , data_start = 1 $
;    , record_start = 0, count = count)
;
;  field_count = n_elements(ht_data.(0)) / count
;  header = header
;
;  return, ht_data.(0)
;end

function nrs_get_bw_lut
  compile_opt idl2, logical_predicate

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
		
		return, lut
end

function get_yellow_rainbow_lut
  compile_opt idl2, logical_predicate

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
		
		return, lut
end

function nrs_get_custom_lut, filename
  compile_opt idl2, logical_predicate
  
  color_table = strtrim(filename, 2)
  if strlen(color_table) le 0 then return, []
  
  ent = nrs_read_table(color_table, valid = valid)
  sz = size(ent, /dim)
  cols = sz[0]
  rows = sz[1]
  if (cols eq 3) && (rows eq !d.table_size) then begin
    ; the table is already a color lut, set special colors and return
    lut = ent
    lut[*, 0 : rows - 5] = congrid(ent, 3, rows - 4)
    lut[*, rows - 4] = ent[*, -1]
    
    lut[*, -3:-1] = $
        [[128,128,128], $  ; gray
         [160,160,160], $  ; light gray
         [255,255,255]]    ; white
    return, lut
  endif

  ; Colors are defined by breaks/limits; calculate the LUT
  color_breaks = float(ent[0, *])
  color_breaks /= max(color_breaks) ; normalize 
  color_limits = fix(ent[1:3, *])
  
  return, nrs_build_lut(color_limits, breaks = color_breaks)
end

function nrs_build_lut, rgb_limits, breaks = breaks, is_perc = is_perc
  compile_opt idl2, logical_predicate
  
  lut = []
  sz = size(rgb_limits)
  ncol = sz[0]
  nrec = sz[2]
  
  if ncol ne 2 then return, lut
  if sz[1] ne 3 then return, lut
  if nrec lt 2 then return, lut
  
  tbl_size = !d.table_size
  
  is_perc = keyword_set(is_perc)
  if is_perc then rgb = 1.0 * rgb_limits * 252 / 100 $
  else rgb = rgb_limits < tbl_size - 4 ; 3 colors are reserved
  
  if n_elements(breaks) eq 0 then breaks = fix((tbl_size - 4) * findgen(nrec) / (nrec - 1)) $
  else breaks = fix(breaks * (tbl_size - 4)) 
  
  steps = ceil((tbl_size - 3) / (nrec - 1))
  lut = fltarr(3, tbl_size)
  for i = 0, nrec - 2 do begin
    brlow = breaks[i]
    brhigh = breaks[i + 1]
    lowcol = rgb_limits[*, i]
    highcol = rgb_limits[*, i + 1]
    steps = brhigh - brlow + 1
    lut[*, brlow : brhigh] = interpolate([[lowcol], [highcol]], findgen(steps) * 1 / (steps - 1))
  endfor

  ; special colors:
  lut[*, tbl_size - 3] = [128,128,128]  ; gray
  lut[*, tbl_size - 2] = [160,160,160]  ; light gray
  lut[*, tbl_size - 1] = [255,255,255]  ; white

  return, lut
end

pro draw_symbol, num, thick
	case abs(num) of 
    0 : begin ; diamond
          x = [-0.7, 0.0,  0.7,  0.0,  -0.7]
          y = [ 0.0, 0.75, 0.0, -0.75,  0.0]
          usersym, x, y, /fill
        end 
	  1 : begin ; square
  	      x = [-0.5, -0.5, 0.5, 0.5, -0.5]
          y = [-0.5, 0.5, 0.5, -0.5, -0.5]
          usersym, x, y, /fill
    	  end 
	  2 : begin ; up-triangle
      		x = [-0.6, 0.0,  0.6, -0.6]
      		y = [-0.6, 0.6, -0.6, -0.6]
      		usersym, x, y, /fill
        end
    3 : begin ; down-triangle
          x = [0.6,  0.0, -0.6, 0.6]
          y = [0.6, -0.6,  0.6, 0.6]
          usersym, x, y, /fill
        end
    4 : begin ; circle
          a = findgen(17) * (!pi*2/16.)
          usersym, 0.5 * cos(a), 0.5 * sin(a), /fill
        end
    5 : begin ; +
          x = [-0.5, -0.1, -0.1, 0.1, 0.1, 0.5,  0.5,  0.1,  0.1, -0.1, -0.1, -0.5, -0.5]
          y = [ 0.1,  0.1,  0.5, 0.5, 0.1, 0.1, -0.1, -0.1, -0.5, -0.5, -0.1, -0.1, -0.1]
          usersym, x, y, /fill
        end
    6 : begin ; X
          x = [-0.6, -0.2, -0.6, -0.5, 0.0, 0.5, 0.6, 0.2,  0.6,  0.5,  0.0, -0.5, -0.6]
          y = [-0.5,  0.0,  0.5,  0.5, 0.2, 0.5, 0.5, 0.0, -0.5, -0.5, -0.2, -0.5, -0.5]
          usersym, x, y, /fill
        end
    else: begin
      a = findgen(17) * (!pi*2/16.)
      usersym, 0.5 * cos(a), 0.5 * sin(a)
    endelse
  endcase
end

