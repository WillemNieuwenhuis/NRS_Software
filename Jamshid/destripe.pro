; Author: Willem Nieuwenhuis, march 2009
; Destripe a band
; Algorithm [1]:
;		val_new[i,j,k] = (val_old[i,j,k] - mu[j,k]) * sig[k] / sig[j, k] + mu[k]
;
; Algorithm [2]:
;		val_new[i,j,k] = (val_old[i,j,k] - mu[j,k]) * sig[m] / sig[j, k] + mu[m]
;
; Algorithm [3]:
;		val_new[i,j,k] = (val_old[i,j,k] - mu[j,k]) * sig[c] / sig[j, k] + mu[c]
;
; where:
;	i = row
;	j = column
;	k = band
;	mu[j, k], sig[j, k] = mean and std of all original values in column j in band k
;	mu[k],    sig[k]    = mean and std of all original values in band k
;	mu[m],    sig[m]    = mean and std of all original values in all bands
;	mu[c],    sig[c]    = mean and std of all original values in band c (for [3])
;
; Remove striping by applying [1], [2] or [3]
; The keywords 'm' and 'sd' are set (both are needed) to the mean/stddev of the data
; as described by the algorithms:
;	[1]	mean/stddev of the band to be destriped
;	[2]	mean/stddev of the entire image
;	[3]	mean/stddev of the selected neighbor band
;
; Parameters:
;	band:		The band data to correct
;	b:			Band number in the image
;	bi:			The band counter indicating the number of bands having been handled
;	nl:			The number of lines
;	ns:			The number of samples
;	band_stat:	Storage for the calculated mean and standard deviation information (band/image)
;	col_stats:	Storage for the calculated mean and standard deviation information (columns)
;
; Keywords:
;	m:			The mean of the image/(neigbor)band
;	sd:			The standard deviation of the image/(neigbor)band
;
; Return:
;	The band after destriping, or 0 (zero) if the mean / stddev are not set
function destripe_band, band, b, bi, nl, ns, m = m_in, sd = sd_in, band_stat = bs, col_stats = cs
	; check input keywords
	cnt = 0
	if n_elements(m_in) gt 0 then begin
		m = m_in	; statistics for [2, 3]
		cnt++
	endif

	if n_elements(sd_in) gt 0 then begin
		sd = sd_in	; statistics for [2, 3]
		cnt++
	endif
	
	if cnt ne 2 then return, 0

	; make the new output (simply make a copy)
	dr = band

	if n_elements(bs) gt 0 then begin
		bs[*, bi] = [b + 1, 0, 0, m, sd]	; b + 1 to get 1-based again
	endif
	; do the destriping
	for s = 0, ns - 1 do begin
		; calculate column statistics
		m_jk = mean(band[s, *])
		sd_jk = stddev(band[s, *])
		if n_elements(bs) gt 0 then begin
			bci = bi * ns + s
			cs[*, bci] = [b + 1, s, m_jk, sd_jk]	; b + 1 to get 1-based again
		endif

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
;	type:	1 for formula [1], 2 for formula [2] or 3 for formula [3]
; Keywords
;	bands:		list all the bands to destripe
;	neighbor:	band to get the statistics from for formula [3]
;	stats:		If set then store the calculated mean and standard deviation information
;
; Examples;
; 	destripe, 2						; destripe all bands in the image with formula [2]
;	destripe, 2, bands=4			; only destripe band 4 in the image with formula [2]
;	destripe, 1, bands=[4, 5, 19]	; only destripe bands 4, 5, 19 in the image with formula [1]
;	destripe, 3, neighbor=5			; destripe all bands with formula [3] using band 5 for stats
;	destripe, 3, neighbor=5, /stats	; destripe with formula [3] using band 5, and create statistics
;
; The module will ask for a image filename
; The band numbers are 1-based, so can range from [1..#bands]
; The statistics files are stored in the same folder as the input
; The name of the column statistics: <input name>_colstats_<type>.csv
; The name of the band statistics: <input name>_bandstat_<type>.csv
; For example: "try__bandstat_3.csv", when input is "try_" and algorithm = "3"
pro destripe, type, bands = bands, neighbor = neighbor, stats = stats
	if (n_params() eq 0) then begin
		res = dialog_message('No algorithm specified; select 1, 2 or 3', /error)
		return
	endif

	if (type ne 1) and (type ne 2) and (type ne 3) then begin
		res = dialog_message('Wrong algorithm specified; select 1, 2 or 3', /error)
		return
	endif

	if (type eq 3) and (n_elements(neighbor) eq 0) then begin
		res = dialog_message('Type 3 algorithm requires the neighbor keyword to be set', /error)
		return
	endif
	if (type eq 3) and (n_elements(neighbor) gt 1) then begin
		res = dialog_message('The neighbor value must be a single band number', /error)
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

	if type eq 2 then begin
		m_img = mean(images)
		sd_img = stddev(images)
	endif else if type eq 3 then begin
		m_img = mean(images[*, *, neighbor - 1])
		sd_img = stddev(images[*, *, neighbor - 1])
	endif

	band_arr = intarr(nb)
	if n_elements(bands) gt 0 then begin
		bands[*] -= 1	; convert from 1-based to 0-based for internal use
		band_arr[bands] = 1
	endif else begin
		band_arr[*] = 1
	endelse

	do_stats = 0
	if keyword_set(stats) then begin
		do_stats = 1
		band_count = total(band_arr)
		band_stat = fltarr(5, band_count)	; [btbd, type, s_src, m, std]
		col_stats = fltarr(4, ns * band_count)	; [btbd, c, m, std]
	endif

	envi_report_init, 'Destriping', title = 'Destripe', base = base
	envi_report_inc, base, nb
	bi = 0
	for b = 0, nb - 1 do begin
  		envi_report_stat, base, b, nb
  		; only handle selected bands
		if band_arr[b] eq 1 then begin
			if type eq 1 then begin
				m_img = mean(images[*, *, b])
				sd_img = stddev(images[*, *, b])
	  		endif
	  		if do_stats eq 1 then begin
  				images[*, *, b] = destripe_band(images[*, *, b], b, bi, nl, ns, m = m_img, sd = sd_img $
  						, band_stat = band_stat, col_stats = col_stats)
  				band_stat[1, bi] = type
  				switch type of
  					1:	begin
  							band_stat[2, bi] = b + 1	; make 1-based
  							break
  						end
  					2:  begin
  							band_stat[2, bi] = -1
  							break
  						end
  					3:  begin
  							band_stat[2, bi] = neighbor
  							break
  						end
  				endswitch
	  		endif else begin
  				images[*, *, b] = destripe_band(images[*, *, b], b, bi, nl, ns, m = m_img, sd = sd_img)
  			endelse
  			bi++
		endif
	endfor
	envi_report_init, base = base, /finish

	; make output filename
	outname = filename + '_out_' + string(type, format='(i0)')

	; write the result to disk
	envi_write_envi_file, images, out_name = outname

	; now handle statistics output if calculated
	if do_stats eq 1 then $
		writeStatistics, band_stat, col_stats, type, filename
end
