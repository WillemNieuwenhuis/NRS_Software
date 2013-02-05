; ***********************************************************************
; s13aaf.pro
; ***********************************************************************
; Exponential integral function
; y = s13aaf(x) is the integral from x to Inf of (exp(-t)/t) dt
; ***********************************************************************
; Translated from Matlab to IDL feb 2008, ITC NRS department

function s13aaf, k
	sz = n_elements(k)
	x = dblarr(sz)
	y = dblarr(sz)
	f = dblarr(sz)
	sel4 = where(k le 4, sel4_count)
	sel85 = where(k ge 85, sel85_count)
	sel4to85 = where(k gt 4 and k lt 85, sel4to85_count)

	if sel4_count ne 0 then begin
		x[sel4] = 0.5 * k[sel4] - 1
		y[sel4] = -3.60311230482612224d-13 * x[sel4] + 3.46348526554087424d-12
		y[sel4] = y[sel4] * x[sel4] - 2.99627399604128973d-11
		y[sel4] = y[sel4] * x[sel4] + 2.57747807106988589d-10
		y[sel4] = y[sel4] * x[sel4] - 2.09330568435488303d-9
		y[sel4] = y[sel4] * x[sel4] + 1.59501329936987818d-8
		y[sel4] = y[sel4] * x[sel4] - 1.13717900285428895d-7
		y[sel4] = y[sel4] * x[sel4] + 7.55292885309152956d-7
		y[sel4] = y[sel4] * x[sel4] - 4.64980751480619431d-6
		y[sel4] = y[sel4] * x[sel4] + 2.63830365675408129d-5
		y[sel4] = y[sel4] * x[sel4] - 1.37089870978830576d-4
		y[sel4] = y[sel4] * x[sel4] + 6.47686503728103400d-4
		y[sel4] = y[sel4] * x[sel4] - 2.76060141343627983d-3
		y[sel4] = y[sel4] * x[sel4] + 1.05306034687449505d-2
		y[sel4] = y[sel4] * x[sel4] - 3.57191348753631956d-2
		y[sel4] = y[sel4] * x[sel4] + 1.07774527938978692d-1
		y[sel4] = y[sel4] * x[sel4] - 2.96997075145080963d-1
		y[sel4] = y[sel4] * x[sel4] + 8.64664716763387311d-1
		y[sel4] = y[sel4] * x[sel4] + 7.42047691268006429d-1
		f[sel4] = y[sel4] - alog(k[sel4])
	endif
	if sel85_count ne 0 then begin
		f[sel85] = 0
	end
	if sel4to85_count ne 0 then begin
		x[sel4to85] = 14.5 / (k[sel4to85] + 3.25) - 1
		y[sel4to85] = -1.62806570868460749d-12 * x[sel4to85] - 8.95400579318284288d-13
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 4.08352702838151578d-12
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.45132988248537498d-11
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 8.35086918940757852d-11
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 2.13638678953766289d-10
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.10302431467069770d-9
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 3.67128915633455484d-9
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.66980544304104726d-8
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 6.11774386401295125d-8
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 2.70306163610271497d-7
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.05565006992891261d-6
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 4.72090467203711484d-6
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.95076375089955937d-5
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 9.16450482931221453d-5
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 4.05892130452128677d-4
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 2.14213055000334718d-3
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 1.06374875116569657d-2
		y[sel4to85] = y[sel4to85] * x[sel4to85] - 8.50699154984571871d-2
		y[sel4to85] = y[sel4to85] * x[sel4to85] + 9.23755307807784058d-1
		f[sel4to85] = exp(-k[sel4to85]) * y[sel4to85] / k[sel4to85]
	endif

	return, f
end
