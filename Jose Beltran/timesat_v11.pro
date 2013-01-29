pro tmsat_EstimatedTime_strings, t1, t2, nelem = nelem, s_persec, s_estim, is_end
	timediff = t2 - t1
  estimatedHours = timediff
	nitems = 1
	if n_elements(nelem) eq 1 then nitems = nelem
  estimatedHours = timediff * nitems / 3600
    
	hour = floor(estimatedHours)
	mins = (estimatedHours - hour) * 60
	minutes = floor(mins)
	secs = (mins - minutes) * 60
	s_persec = 'Elapsed time by cleaning one pixel: ' + string(timediff, format='(i0)') + ' seconds'
	if is_end eq 0 then begin
  	s_estim = 'Estimated running time: ' $
  			+ string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)') $
  			+ ' hours to finish'
  endif else begin
    s_estim = 'Actual running time: ' $
        + string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)') $
        + ' hours.'
  endelse
end

function time_to_string, seconds
	hours = (seconds / 3600) mod 24	; only interested in day time
	hour = floor(hours)
	mins = (hours - hour) * 60
	minutes = floor(mins)
	secs = (mins - minutes) * 60
	return, string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)')
end

pro tmsat_printEstimatedTime, t1, t2, nelem = nelem
	tmsat_EstimatedTime_strings, t1, t2, nelem = nelem, s_persec, s_estim

	print, s_persec
	print, s_estim
end

; return cleaned up timeseries, filtered and without spikes
function tmsat_handle_timeseries, y, win, nptperyear, spikecutoff, forceUpperEnvelope, lastIterationLikeTIMESATfit, missingdata
	nb = n_elements(y)
	w = fltarr(nb) + 1
	; The accepted range of values are 2:254 all other values are set to zero.
	wzi = where(y lt 2)
	if wzi[0] ne -1 then w[wzi] = 0

	; Time-series with too many data values (>75%) with zero weight are not processed
	missingdata = 0
	if n_elements(where(w eq 0)) ge floor(3 * nb / 4) then missingdata = 1

	ptyear3 = floor(nptperyear / 3)
	for k = 0, nb - ptyear3 do begin
		sk = indgen(ptyear3) + k
		if abs(total(w[sk])) eq 0 then missingdata = 1
	endfor

	if missingdata eq 0 then begin
		; Identify spikes in the time-series and set the corresponding weights to zero
		spikes = tmsat_spike(y, w, nptperyear, cutoff = spikecutoff)
		ws = w
		wsi = where(spikes eq 1)
		if wsi[0] ne -1 then ws[wsi] = 0

		; Iterative Savitzky-Golay filtering to adopt to upper (or lower) envelope --
		; of the time-series.
		y1 = tmsat_savgol(y, ws, win $
			, forceUpperEnvelope, lastIterationLikeTIMESATfit $
			)
	endif else begin
		y1 = fltarr(nb, win[n_elements(win) - 1]) ; The profile is set to zero.
	endelse

	return, y1
end

; main routine
pro timesat_v11, src_filename, out_filename $
               , vr_min = vr_min, vr_max = vr_max $
               , prog_obj = progressBar, cancelled = cancelled $
               , win, forceUpperEnvelope, lastIterationLikeTIMESATfit, disp_win = disp_win
  compile_opt idl2, logical_predicate
  
  envi_open_file, src_filename, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = dialog_message('Could not open input file', /error)
    return
  endif

	envi_file_query, fid, nb = nb, nl = nr, ns = nc, data_type = dt, data_ignore_value = undef
	has_undef = (size(undef, /type) eq dt) or ( (size(undef, /type) eq 5) and undef ne 1e34) 

  if dt ne 1 then begin
    if n_elements(vr_min) eq 0 then vr_min = -1.0
    if n_elements(vr_max) eq 0 then vr_max = 1.0
  endif
	spikecutoff = 0.5

	; Initiate vectors that will hold the time series for a pixel
	y = fltarr(nb)

	nptperyear = 36
	openw, unit, out_filename, /get_lun

  nrs_set_progress_property, progressBar, /start
	displayEstimatedTime = 1
	t0 = systime(1)
	for row = 0L, nr - 1 do begin
      void = nrs_update_progress(progressBar, row, nr)

	    rmatrix = envi_get_slice(fid = fid, /bil, line = row, xs = 0, xe = nc - 1)
	    if dt ne 1 then begin
	      cnt_undef = 0
	      if has_undef then uix = where(rmatrix eq undef, cnt_undef)
	      rmatrix = byte( (rmatrix - vr_min) / (vr_max - vr_min) * 254.0 + 0.5) + 1
	      if cnt_undef gt 0 then rmatrix[uix] = 0
	    endif

	    for col = 0, nc - 1 do begin
	    	if col mod 20 eq 0 then begin
	    	  if nrs_update_progress(progressBar, row, nr, cancelled = cancelled) then begin
            close, unit  ; close output
            free_lun, unit
            file_delete, out_filename, /noexpand_path, /allow_nonexistent, /quiet
            return
	    	  endif
				endif

        y = reform(rmatrix[col, *], nb)
        t3 = systime(1)
        y1 = tmsat_handle_timeseries(y, win, nptperyear, spikecutoff, forceUpperEnvelope, lastIterationLikeTIMESATfit, missing_data)
        ; estimate runtime but only if processing was done (no missing_data)
        if (displayEstimatedTime eq 1) and (missing_data eq 0) then begin
          t4 = systime(1)

				  if n_elements(disp_win) gt 0 then begin
      		  tmsat_EstimatedTime_strings, t3, t4, nelem = long(nr) * nc, s_persec, s_estim, 0
            s_dt = 'Start time: ' + systime(0)
            widget_control, disp_win, set_value = s_dt + ', ' + s_estim, /append
          endif

          displayEstimatedTime = 0
        endif

        rmatrix[col, *] = y1[*, n_elements(win) - 1]
	    endfor ; col

      if dt ne 1 then begin
        rmatrix = fix( float(rmatrix - 1) / 254 * (vr_max - vr_min) + vr_min, type = dt)
        if cnt_undef gt 0 then rmatrix[uix] = undef 
      endif

      writeu, unit, rmatrix  ; bil storage
	endfor ; rows

  if n_elements(disp_win) gt 0 then begin
    te = systime(1)
    tmsat_EstimatedTime_strings, t0, te, s_persec, s_estim, 1
    s_dt = 'End time: ' + systime(0)
    widget_control, disp_win, set_value = s_dt + ', ' + s_estim, /append
  endif
	progressBar->update, 100, text = 'Writing output...'

	if forceUpperEnvelope eq 1 then begin
	    if lastIterationLikeTIMESATfit eq 1 then $
	        description = 'SAVGOL-TIMESAT with all but the last, UENV-forced' $
	    else $
	        description = 'SAVGOL-TIMESAT with forced UENV'
	endif else $
	    description = 'Upper Envelope (UENV) Savitzky-Golay filter (SAVGOL) TIMESAT-based approach'

  ; write the header for the data file
  ; first get additional details
  envi_file_query, fid, fname = filename $
        , fwhm = fwhm, wl = wl, wavelength_units = wu $
        , bnames = bnames $
        , byte_swap = byte_swap $
        , xstart = xs, ystart = ys
  mapinfo = envi_get_map_info(fid = fid, undefined = undefined)
  if undefined eq 1 then dummy = temporary(mapinfo)
  
  envi_setup_head, fname = out_filename, data_type = dt, /write $
          , interleave = 1 $  ; BIL
          , nb = nb, nl = nr, ns = nc $
          , descrip = description $
          , map_info = mapinfo $
          , fwhm = fwhm, wl = wl, wavelength_units = wu $
          , bnames = bnames $
          , data_ignore_value = undef $
          , xstart = xs, ystart = ys

  close, unit
  free_lun, unit  ; close output
end

