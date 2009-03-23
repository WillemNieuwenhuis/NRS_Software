; Author: Willem Nieuwenhuis, februari 2009
; function to find the corners of the scene within the image
; Assumption: the pixels not belonging to the scene are zeroes
;
; Parameters:
;	width, height:	the size of the image
;	image:			the image to find the corners in
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

; procedure to write the details of the affine matrix
; to a log file (will be overwritten each time!)
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

; Do the resample. The same matrix is used for all scenes in the file
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

	; start the resampling
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

	; return the resampled scenes
	return, newimg
end


; helper function to load an image and write the result back to a (new) file
; can be used as a test
pro process_image_One
	; open a file
	envi_select, fid = fid, title = 'Select a multi-spectral image'

	; get the input filename and dimensions
	envi_file_query, fid, fname = filename, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt

	images = make_array(ns, nl, nb, type = dt)
	for pos = 0, nb - 1 do begin
		images[*, *, pos] = envi_get_data(fid = fid, dims = dims, pos = pos)
	endfor

	; find the corners of the scene(s) in the first scene
	bounds = find_corners(ns, nl, images[*, *, 0])

	; use the boundaries to calculate the affine transformation matrix
	; and do the resampling
	outimage = rotate_shift(ns, nl, nb, images, bounds, dt)

	; make output filename
	outname = filename + '_out'

	; write the result to disk
	envi_write_envi_file, outimage, out_name = outname

end