; ***********************************************************************
; tav.pro
; ***********************************************************************
; Stern F. (1964), Transmission of isotropic radiation across an
; interface between two dielectrics, Appl. Opt., 3(1):111-113.
; Allen W.A. (1973), Transmission of isotropic light across a
; dielectric surface in two and three dimensions, J. Opt. Soc. Am.,
; 63(6):664-666.
; ***********************************************************************
; Translated from Matlab to IDL feb 2008, ITC NRS department

function tav, teta, ref
	s  =  n_elements(ref)
	teta = teta * !pi / 180
	r2 = ref ^ 2
	rp = r2 + 1
	rm = r2 - 1
	a = (ref + 1) ^ 2 / 2
	k = -(r2 - 1) ^ 2 / 4
	ds = sin(teta)

	if teta eq 0 then begin
		f = 4 * ref / (ref + 1) ^ 2
	endif else begin
		if teta eq !pi / 2 then begin
			b1 = dblarr(s)
		endif else begin
			b1 = sqrt((ds ^ 2 - rp / 2) ^ 2 + k)
		endelse
	endelse
	b2 = ds ^ 2 - rp / 2
	b = b1 - b2
	ts = ((k ^ 2) / (6 * b ^ 3) + k / b - b / 2) - ((k ^ 2) / (6 * a ^ 3) + k / a - a / 2)
	tp1 = -2 * r2 * (b - a) / (rp ^ 2)
	tp2 = -2 * r2 * rp * alog(b / a) / rm ^ 2
	tp3 = r2 * (1 / b - 1 / a) / 2
	tp4 = 16 * (r2 ^ 2) * (r2 ^ 2 + 1) * alog((2 * rp * b - rm ^ 2) / (2 * rp * a - rm ^ 2)) / ((rp ^ 3) * rm ^ 2)
	tp5 = 16 * (r2 ^ 3) * (1 / (2 * rp * b - rm ^ 2) - 1 / (2 * rp * a - rm ^ 2)) / rp ^ 3
	tp = tp1 + tp2 + tp3 + tp4 + tp5
	f = (ts + tp) / (2 * ds ^ 2)

	return, f
end
