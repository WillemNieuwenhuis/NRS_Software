;+
; :params:
;   y : in, required
;     1D array with nb items, the timeseries
;   w : in, required
;     1D array with nb items, the mask (1 or 0)
;   nptperyear : in, required
;     Number of NDVI images per year
;
; :keywords:
;	  cutoff : in, optional, default = 0.5
;	    threshold value
;
; :returns:
;	  1D array indicating spikes
;
;-
function tmsat_spike, y, w, nptperyear, cutoff = spikecutoff
  compile_opt idl2, logical_predicate
  
	if n_elements(spikecutoff) eq 0 then spikecutoff = 0.5

	; A spikecutoff will be calculated as:
	;		spikecutoff * y[y>0].std().
	; A value of 2 is the normal value for TIMESAT. But originally TIMESAT uses spikecutoff * y.std().
	; I suggest to use only the values of "y > 0" to calculate the distance. A value lower than 1
	; will pick up more spikes. (Jose Beltran)

  nb = n_elements(y)
	spikes = bytarr(nb)
	y_c = y[where(y gt 0)]
	n = n_elements(y_c)
	bias = sqrt(1.0 * (n - 1) / n)  ; used to calc the biased stddev as in python
	ystd = stddev(y_c) * bias	; from unbiased stddev to biased (as in python version)
	distance = spikecutoff * ystd
	swinmax = floor(nptperyear / 7) ; unsure which value to use: maybe 5 or 7 or 10
	leftSlice = indgen(swinmax) + nb - swinmax
	rightSlice = indgen(swinmax)

	yext = [y[leftSlice], y, y[rightSlice]]

	; find single spikes and set weights to zero
	for i = swinmax, nb + swinmax - 1 do begin
		m1 = i - swinmax
		m2 = i + swinmax + 1

		nonzero = where(yext[m1 : m2 - 1] gt 0, nz_cnt)
		med = 0.0
		if nz_cnt gt 0 then begin
			med = median(yext[nonzero + m1])
		endif

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
