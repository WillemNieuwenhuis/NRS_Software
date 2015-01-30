; Parameters
;	y		1D array with nb items, the timeseries
;	w		1D array with nb items, the mask (1 or 0)
;	nptperyear	Number of NDVI images per year
;
; Keywords
;	cutoff		threshold value (default 0.5)
;
; Returns
;	spikes		1D array indicating spikes
;
function nrs_giacis_spike, y, w, nptperyear, cutoff = spikecutoff
	if n_elements(spikecutoff) eq 0 then spikecutoff = 0.5

	; A spikecutoff will be calculated as:
	;		spikecutoff * y[y>0].std().
	; A value of 2 is the normal value for TIMESAT. But originally TIMESAT uses spikecutoff * y.std().
	; I suggest to use only the values of "y > 0" to calculate the distance. A value lower than 1
	; will pick up more spikes. (Jose Beltran)

	nb = n_elements(y)
	bias = sqrt(1.0 * (nb - 1) / nb)	; used to calc the niased stddev as in python
	spikes = fltarr(nb)
	y_c = y[where(y gt 0)]
	ymean = mean(y_c)
	ystd = stddev(y_c) * bias	; from unbiased stddev to biased (as in python version)
	distance = spikecutoff * ystd
	swinmax = floor(nptperyear / 7) ; unsure which value to use: maybe 5 or 7 or 10
	leftSlice = indgen(swinmax) + nb - swinmax
	rightSlice = indgen(swinmax)

	wext = [w[leftSlice], w, w[rightSlice]]
	yext = [y[leftSlice], y, y[rightSlice]]

	; find single spikes and set weights to zero
	for i = swinmax, nb + swinmax - 1 do begin
		m1 = i - swinmax
		m2 = i + swinmax + 1

		wind = wext[m1:m2 - 1]
		idx_wext_nonzero = where(wind gt 0)
		if idx_wext_nonzero[0] ne -1 then begin
			indices = idx_wext_nonzero + m1
			med = median(yext[indices])
		endif else $
			med = 0.0

		dev = abs(y[m1] - med)
		avg_y = (1.0* yext[i - 1] + yext[i + 1]) / 2
		max_y = max([yext[i - 1], yext[i + 1]])
		if dev ge distance and $
				( y[m1] lt (avg_y - distance) or $
				  y[m1] gt (max_y + distance) ) then begin
			spikes[m1] = 1
		endif
	endfor

	return, spikes
end
