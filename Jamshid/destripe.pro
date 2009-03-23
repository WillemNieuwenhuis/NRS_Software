; Author: Willem Nieuwenhuis, march 2009
; Destripe a band
; Algorithm [1]:
;		val_new[i,j,k] = (val_old[i,j,k] - mu[j,k]) * sig[k] / sig[j, k] + mu[k]
;
; Algorithm [2]:
;		val_new[i,j,k] = (val_old[i,j,k] - mu[j,k]) * sig[m] / sig[j, k] + mu[m]
;
; where:
;	i = row
;	j = column
;	k = band
;	mu[j, k], sig[j, k] = mean and std of all original values in column j in band k
;	mu[k],    sig[k]    = mean and std of all original values in band k
;	mu[m],    sig[m]    = mean and std of all original values in all bands
;
; Remove striping by applying [1] or [2]
; If the keywords 'm' and 'sd' are set (both are needed) then [2] is
; calculated, otherwise [1] is calculated
; Parameters:
;	band:		The band data to correct
;	nl:			The number of lines
;	ns:			The number of samples
; Keywords:
;	m:			The mean in case image based ([2])
;	sd:			The standard deviation in case image based ([2])
;
; Return:
;	The band after destriping
function destripe_band, band, nl, ns, m = m_in, sd = sd_in
	; check input keywords
	cnt = 0
	if n_elements(m_in) gt 0 then begin
		m = m_in	; statistics for [2]
		cnt++
	endif

	if n_elements(sd_in) gt 0 then begin
		sd = sd_in	; statistics for [2]
		cnt++
	endif

	; make the new output (simply make a copy)
	dr = band

	; calculate the band statistics for [1]
	if cnt ne 2 then begin
		m = mean(band)
		sd = stddev(band)
	endif

	; do the destriping
	for s = 0, ns - 1 do begin
		; calculate column statistics
		m_jk = mean(band[s, *])
		sd_jk = stddev(band[s, *])

		; Apply the formula
		for l = 0, nl - 1 do begin
			dr[s, l] = (band[s, l] - m_jk) * sd / sd_jk + m
		endfor
	endfor

	return, dr
end

; helper function to load an image, destripe and write the result back to a (new) file
; can be used as a test
; Parameter
;	type:	1 for formula [1] or 2 for formula [2]
; Keyword
;	bands:	list all the bands to destripe
;
; Examples;
; 	destripe, 2						; destripe all bands in the image with formula [2]
;	destripe, 2, bands=4			; only destripe band 4 in the image with formula [2]
;	destripe, 1, bands=[4, 5, 19]	; only destripe bands 4, 5, 19 in the image with formula [1]
;
; The module will ask for a image filename
; The band numbers are 0-based, so can range from [0..#bands-1]
pro destripe, type, bands = bands
	if (n_params() eq 0) then begin
		res = dialog_message('No algorithm specified; select 1 or 2', /error)
		return
	endif

	if (type ne 1) and (type ne 2) then begin
		res = dialog_message('Wrong algorithm specified; select 1 or 2', /error)
		return
	endif

	; open a file
	envi_select, fid = fid, title = 'Select a multi-spectral image'
	if fid eq -1 then return

	; get the input filename and dimensions
	envi_file_query, fid, fname = filename, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt

	images = make_array(ns, nl, nb, type = dt)
	for pos = 0, nb - 1 do begin
		images[*, *, pos] = envi_get_data(fid = fid, dims = dims, pos = pos)
	endfor

	m_img = mean(images)
	sd_img = stddev(images)

	band_arr = intarr(nb)
	if n_elements(bands) gt 0 then $
		band_arr[bands] = 1

	envi_report_init, 'Destriping', title = 'Destripe', base = base
	envi_report_inc, base, nb
	for b = 0, nb - 1 do begin
  		envi_report_stat, base, b, nb
  		; only handle selected bands
		if band_arr[b] eq 1 then begin
			if type eq 1 then begin
	  			images[*, *, b] = destripe_band(images[*, *, b], nl, ns)
	  		endif else begin
	  			images[*, *, b] = destripe_band(images[*, *, b], nl, ns, m = m_img, sd = sd_img)
			endelse
		endif
	endfor
	envi_report_init, base = base, /finish

	; make output filename
	outname = filename + '_out_' + string(type, format='(i0)')

	; write the result to disk
	envi_write_envi_file, images, out_name = outname
end
