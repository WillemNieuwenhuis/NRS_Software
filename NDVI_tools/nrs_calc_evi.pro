; calculate EVI:
;	EVI = 2.5 * (nir_band - red_band) / (nir_band + (6 * red_band) - (7.5 * blue_band) + 1)
; parameters:
; 	data 		contains a stack of bands
;	ns, nl		size of a single band
;	red_band	band index in data, for the red component
;	nir_band	band index in data, for the NIR component
;	blue_band	band index in data, for the blue component
;
pro nrs_calc_evi, data, ns, nl, red_band, nir_band, blue_band, evi
	red  = data[*, *, red_band]
	nir  = data[*, *, nir_band]
	blue = data[*, *, blue_band]
	evi = 2.5 * (nir - red) / (nir + (6 * red) - (7.5 * blue) + 1)
end