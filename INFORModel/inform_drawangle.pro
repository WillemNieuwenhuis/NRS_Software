pro inform_drawAngle, draw_pane, x_axis, y_data, angleType, lut_id
  	Widget_Control, draw_pane, Get_Value = dispID
	wset, dispID
    DEVICE, DECOMPOSED = 0
	loadct,lut_id, /silent	; 16 level color lut
	; display the axes
	plot, x_axis $
		, BACKGROUND = 255, COLOR = 0 $
		, /nodata $
		, y_data $
		, xrange = [-90, 90], /xstyle $
		, xtickinterval = 30 $
		, yrange = [0.0, 0.5], /ystyle $
      	, Position = [0.15, 0.15, 0.85, 0.85] $
		, title = 'Directional Reflectance Variation' $
      	, font = 0 $
      	, XTitle = angleType + ' [degree]' $
      	, YTitle = 'Reflectance'

	divs = 6
	div_steps = (2500 - 400) / divs
	ticks = indgen(divs + 1) * div_steps + 400
	tick_names = string(ticks, format = '(i4)')
	colorbar_n, position = [0.87, 0.15, 0.90, 0.85] $
		, BACKGROUND = 255, COLOR = 0 $
		,/vertical, /right, font = 0 $
		, divisions = divs, ticknames = tick_names
end