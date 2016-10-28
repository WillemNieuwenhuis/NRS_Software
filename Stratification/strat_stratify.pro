; fid[input]		the file ID of the ndvi / RDP image file
; images[output]	the timeseries of maps in the image file
; dim[output]		the dimensions of the timeseries
; is_valid[output]	true(1) if datatype =float or double, false(0) otherwise
pro strat_read, fid, images, dim, is_valid
	envi_file_query, fid, nb = nb, nl = nl, ns = ns, data_type = dt, dims = dims
	if dt eq 4 or dt eq 5 then begin
		is_valid = 1
		images = dblarr(nb, ns, nl)
		for time = 0, nb - 1 do begin
			images[time,*,*] = envi_get_data(fid = fid, dims = dims, pos = time)
		endfor
	endif else begin
		is_valid = 0
	endelse
	dim = [ns, nl]
end

; fid[input]		the file ID of the DEM image file
; image[output]		the height values
; target_dims[input] the dimensions (columns, rows) to resize to if needed
pro strat_read_dem, fid, image, target_dims
	envi_file_query, fid, nb = nb, nl = nl, ns = ns, data_type = dt, dims = dims

	image = envi_get_data(fid = fid, dims = dims, pos = 0)

	; resize if the image does not have the target dimensions
	if ns ne target_dims[0] or nl ne target_dims[1] then begin
		orig = image
		image = congrid(orig, target_dims[0], target_dims[1])
	endif
end

; images[input]		timeseries of ndvi (or RDP) images
; dem[input]		DEM of the same area / resolution as images
; step[input]		height step for stratification; if set to 0 (zero) defaults to 100 m
; matrix[output]	NDVI / RDP output stratification matrix (time x ndvi[height])
; tranq[input]		progress window
pro strat_stratify, images, dem, step, matrix, tranq
	minH = min(dem, max = maxH)
	if step eq 0 then step = 100

	lines = 1 + round(((maxH - minH) / step))
	dims = size(images)
	dimnr = dims[0]
	cols = dims[1]	; #time moments == nb

	matrix = dblarr(cols, lines)

	height = minH
	line = 0
	tranqstep = 100 / lines
  envi_report_inc, tranq, tranqstep
	
	while height lt maxH do begin
		envi_report_stat, tranq, line , lines, cancel = cancel
		higher = height + step
		mask = where(dem ge height and dem lt higher, count)
		if count gt 0 then begin
			for time = 0, cols - 1 do begin
				img = images[time,*,*]
				imgmask = img[mask]
				matrix[time, lines - line - 1] = mean(imgmask)
			endfor
		endif
		height = higher
		line += 1
	endwhile
end

;+
; :Description:
;    Stratify a timeseries along the Y / latitude direction. The matrix returned has
;    dimensions (X x Y) equal to num_bands x steps.
;
; :Params:
;    image : in, required
;      Name of the input time series
;    steps : in, required
;      The number of lines in the stratication matrix
;
; :Keywords:
;    matrix : out
;      The output of the stratification.
;    cancelled : out
;      indicate error or user abort if set
;    prog_obj : in
;      Progressbar object for progress indication, can be NULL
;      in which case no progress is displayed
;
; :Author: nieuwenhuis
; 
; :History:
;   sept 2012: Stratification in latitude
;-
pro nrs_stratify_timeseries, image, steps, matrix = matrix, cancelled = cancelled, prog_obj = prog_obj 
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return

  envi_file_query, fid, nb = nb, nl = nl, ns = cols, data_type = dt, dims = dims

  cancelled = 0
  
  nrs_set_progress_property, prog_obj, title = 'Calculate stratification', /start
  
  matrix = dblarr(nb, steps)

  ix = lindgen(steps + 1) * nl / steps
  line_step = max(ix - shift(ix, 1))
  images = dblarr(cols, nb, line_step)
  pos = indgen(nb)
  for i = 0, steps - 1 do begin
    if nrs_update_progress(prog_obj, i, steps, cancelled = cancelled) then return
    
    lin_s = ix[i]
    lin_e = ix[i + 1]
    for l = lin_s, lin_e - 1 do begin
      images[*, *, l - lin_s] = envi_get_slice(fid = fid $
                              , line = l $
                              , xs = 0, xe = cols - 1 $
                              , pos = pos, /bil) 
    endfor
    
    mask = where(images le 0, count)
    mask2 = where(images gt 1, count2)
    mask = [mask, mask2]
    count += count2
    if count gt 0 then begin
      if dt eq 4 then images[mask] = !values.f_nan
      if dt eq 5 then images[mask] = !values.d_nan
    endif

    img = images[*, *, 0 : lin_e - lin_s - 1]
    if size(img, /n_dim) lt 3 then $
      inter = temporary(img) $
    else $
      inter = mean(img, dim = 3, /nan)
    matrix[*, i] = mean(inter, dim = 1, /nan)
  endfor

end