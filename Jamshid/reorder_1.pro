;++++++++++++++++++
; reorder_1: Auto rotate image data
; parameters:
;	image:	multi-spectral image or timeseries (3D matrix)
; returns:
;	A 3D matrix with the same dimensions and type as the input, where
;	the images are put strait (no coordinates) and moved to the upper-left
;
; The module finds the orientation of the data and rotates it, so the sides are
; horizontally and vertically.
;
;------------------
function reorder_1, image, stop_at_min
	output = image
	tr_out = transpose(output, [1, 0, 2])

	; Get image info
	dims = size(image, /dimens)
	width = dims[0]        ; Col
	height = dims[1]       ; Row
	numbands = dims[2]     ; band

	prev_shift = 0
	dum_shift = width
	shift_x = 0
	shift_y = 0

	for band = 0, numbands - 1 do begin
		; 1. execute horizontal shift on each band
		hor_shift, width, height, band, image, output, prev_shift, shift_x, shift_y

;		; 2. execute vertical shift on each band
;		; first rotate bands
;		trans = transpose(output, [1, 0, 2])
;		; do the horizontal shift
;		hor_shift, height, width, band, trans, tr_out, dum_shift
;		; reverse transpose
;		output = transpose(tr_out, [1, 0, 2])
    endfor

	return, output
end

function reorder_2, image
	output = image

	; Get image info
	dims = size(image, /dimens)
	width = dims[0]        ; Col
	height = dims[1]       ; Row
	numbands = dims[2]     ; band

	prev_shift = 0
	dum_shift = width
	shift_x = 0
	shift_y = 0

;	for band = 0, numbands - 1 do begin
;		rotate_by_shift, width, height, band, image, output, prev_shift, shift_x, shift_y
;    endfor
	left = 0
	right = 0
	for line = 0, height - 1 do begin
		left = 0
		while left lt width do begin
			if image[left, line, 0] ne 0 then break
			left += 1
		endwhile

		if (left ne 0) and (left ne width) then begin
			right = width - 1
			while right gt left do begin
				if image[right, line, 0] ne 0 then break
				right -= 1
			endwhile
		endif
	endfor
	tp1_x = col
	tp1_y = line
	for line = line, height - 1 do begin
		left = 0
		while left lt width do begin
			if image[left, line, 0] ne 0 then break
			left += 1
		endwhile
		if (left eq 0) or (left eq width) then break
	endfor

	return, output
end

pro find_shift_steps, width, height, image, shift_x, shift_y
	shift_x = 0
	shift_y = 0
	col_prev = 0
	col_prev_ndx = 0
	; find first line with non-zero values
	for line = 0, height - 1 do begin
		col = 0
		while col lt width do begin
			if image[col, line] ne 0 then break
			col += 1
		endwhile
		if (col ne 0) or (col ne width) then break
	endfor
	; determine the shift factors (integer values)
	hor_shift = col
	line_ndx = line
	for line = line, height - 1 do begin
		col = 0
		while col lt width do begin
			if image[col, line] ne 0 then break
			col += 1
		endwhile
		if ((col - hor_shift) ne 0) then begin
			shift_x = col - hor_shift
			shift_y = line - line_ndx
		endif
		; find first non-zero value
		if (col lt width) and (col ne col_prev) then begin
			col_prev = col
			col_prev_ndx = line
		endif
	endfor
end

; In each line shift all leading zeroes to the end of each line
pro hor_shift, width, height, band, image, output, prev_shift, shift_x, shift_y
	for line = 0, height - 1 do begin
		; find first non-zero value
		col = 0
		while col lt width do begin
			if image[col, line, band] ne 0 then break
			col += 1
		endwhile
		; update the output image
		to_shift = width - col
		if to_shift lt prev_shift then to_shift = prev_shift
		if (to_shift) ge prev_shift then begin
			output[*, line, band] = shift(image[*, line, band], to_shift)	; shift zeroes to the end
			if (to_shift) gt 0 then prev_shift = to_shift
		endif
	end
end

function find_corners, width, height, image
	bounds = lonarr(8)

	; find first line with non-zero values
	for line = 0, height - 1 do begin
	    col_sel = where(image[*, line] gt 0, count)
	    if count eq 0 then continue

		bounds[0] = col_sel[0]
		bounds[1] = line
		break
	end

	; find last line with non-zero values
	for line = height - 1, 0, -1 do begin
	    col_sel = where(image[*, line] gt 0, count)
	    if count eq 0 then continue

		bounds[2] = col_sel[count - 1]
		bounds[3] = line
		break
	end

	; find first column with non-zero values
	for col = 0, width - 1 do begin
	    line_sel = where(image[col, *] gt 0, count)
	    if count eq 0 then continue

		bounds[4] = col
		bounds[5] = line_sel[count - 1]
		break
	end

	; find last column with non-zero values
	for col = width - 1, 0, -1 do begin
	    line_sel = where(image[col, *] gt 0, count)
	    if count eq 0 then continue

		bounds[6] = col
		bounds[7] = line_sel[count - 1]
		break
	end

	return, bounds
end

pro output_convert_log, bounds, xsize, ysize, a11, a12, a21, a22, x1, y1
	OPENW, outunit, 'convert.log', /GET_LUN

	printf, outunit, bounds, format ='(2(i))'
	printf, outunit, 'x size = ', xsize
	printf, outunit, 'y size = ', ysize
	printf, outunit, 'a11 = ', a11
	printf, outunit, 'a12 = ', a12
	printf, outunit, 'a21 = ', a21
	printf, outunit, 'a22 = ', a22

	x = 0
	y = 0
	oldx = a11 * x + a12 * y + x1
	oldy = a21 * x + a22 * y + y1
	printf, outunit, x, y, oldx, oldy, format='("(",2(i-),") --> (",2(i-),")")'
	x = 252
	y = 0
	oldx = a11 * x + a12 * y + x1
	oldy = a21 * x + a22 * y + y1
	printf, outunit, x, y, oldx, oldy, format='("(",2(i-),") --> (",2(i-),")")'
	x = 0
	y = 3625
	oldx = a11 * x + a12 * y + x1
	oldy = a21 * x + a22 * y + y1
	printf, outunit, x, y, oldx, oldy, format='("(",2(i-),") --> (",2(i-),")")'
	x = 252
	y = 3625
	oldx = a11 * x + a12 * y + x1
	oldy = a21 * x + a22 * y + y1
	printf, outunit, x, y, oldx, oldy, format='("(",2(i-),") --> (",2(i-),")")'

	FREE_LUN, outunit
end

function rotate_shift, ns, nl, nb, images, bounds, data_type
	x1 = 1.0 * bounds[0]
	y1 = 1.0 * bounds[1]
	x2 = 1.0 * bounds[2]
	y2 = 1.0 * bounds[3]
	x3 = 1.0 * bounds[4]
	y3 = 1.0 * bounds[5]
	x4 = 1.0 * bounds[6]
	y4 = 1.0 * bounds[7]
	; calculate the size of the resampled image
	if y3 lt y4 then begin	; y3 < y4: yaw = left
		xp = x2 + (x3 - x2) * (y4 - y2) / (y3 - y2)
		yq = y4 + (y2 - y4) * (x1 - x4) / (x2 - x4)
		xsize = xp - x3
		ysize = yq - y1
	endif else begin		; y3 > y4: yaw = right
		xp = x2 + (x4 - x2) * (y3 - y2) / (y4 - y2)
		yq = y3 + (y2 - y3) * (x1 - x3) / (x2 - x3)
		xsize = xp - x3
		ysize = yq - y1
	endelse

	a11 = (x4 - x1) / xsize
	a12 = (x3 - x1) / ysize
	a21 = (y4 - y1) / xsize
	a22 = (y3 - y1) / ysize
	; now the conversion from (x,y) in the new image to the old (xo, yo) is described by
	; xo = a11 * x + a12 * y + x1
	; yo = a21 * x + a22 * y + y1

	; log the matrix components and found boundaries to logfile
	output_convert_log, bounds, xsize, ysize, a11, a12, a21, a22, x1, y1

	xs = round(xsize)
	ys = round(ysize)
	newimg = make_array(xsize, ysize, nb, type = data_type)
	rep_steps = ys * nb
	envi_report_init, 'Resampling', title = 'Re-order', base = base
	envi_report_inc, base, rep_steps
	for pos = 0, nb - 1 do begin
	  	for y = 0, ys - 1 do begin
	  		envi_report_stat, base, pos * ys + y, rep_steps
			for x = 0, xs - 1 do begin
				oldx = a11 * x + a12 * y + x1
				oldy = a21 * x + a22 * y + y1
				newimg[x, y, pos] = images[oldx, oldy, pos]
		  	endfor
		endfor
	endfor
	envi_report_init, base = base, /finish

	return, newimg
end


; helper function to load an image and write the result back to a (new) file
; can be used as a test
pro process_image
	; open a file
	envi_select, fid = fid, title = 'Select a multi-spectral image'

	; get the input filename and dimensions
	envi_file_query, fid, fname = filename, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt

	image = make_array(ns, nl, nb, type = dt)
	for pos = 0, nb - 1 do begin
		image[*, *, pos] = envi_get_data(fid = fid, dims = dims, pos = pos)
	end

	; make output filename
	outname = filename + '_out'

	; do your reorder
	outimage = reorder_1(image)

	; write the result to disk
	envi_write_envi_file, outimage, out_name=outname

end

pro process_image_One
	; open a file
	envi_select, fid = fid, title = 'Select a multi-spectral image'

	; get the input filename and dimensions
	envi_file_query, fid, fname = filename, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt

	images = make_array(ns, nl, nb, type = dt)
	for pos = 0, nb - 1 do begin
		images[*, *, pos] = envi_get_data(fid = fid, dims = dims, pos = pos)
	endfor

	bounds = find_corners(ns, nl, images[*, *, 0])

	outimage = rotate_shift(ns, nl, nb, images, bounds, dt)

	; make output filename
	outname = filename + '_out'

	; write the result to disk
	envi_write_envi_file, outimage, out_name = outname

end