; SAILSP2.M SAIL+PROSPECT+SOIL-Modell zur Simulation der spektralen
; Reflektion von 404 bis 2500 nm als Funktion biophysikalischer Parameter
; Die dahinterstehende SAIL Version stammt von F.BARET (III.1993)
; mit SAIL1(l,ala,hot,refl,tran,rs1,rs2,rs3,to,ts,psi,skyl)
; Das PROSPECT Modell schreibt sich refl/tran=LEAF(cab,cw,vai)
; Die spektrale bidirektionelle (rs1), hemisph„risch-direktionelle (rs2) und
; bihemisph„rische Bodenreflexion (rs3) wird ?ber das SOILSPEC-Programm
; simuliert SOIL(ts,to,psi).   Insgesamt ergibt sich:
; ----------- SAILSPEC(l,ala,hot,cab,cw,vai,to,ts,psi,skyl) --------------
; l     : LAI
; ala   : angle moyen d'inclinaison des feuilles
; hot   : parametre hotspot: rapport entre diametre feuille et hauteur strate
; refl  : reflectance des feuilles
; tran  : transmittance des feuilles
; to    : angle zenithal d'obsevation
; ts    : angle zenithal solaire
; psi   : angle azimutal entre observation et soleil
; skyl  : diffuser Strahlungsanteil
;
; Für infinite Reflexion (rc)
; Clement Atzberger im November 1995
; Translated from Matlab to IDL feb 2008, ITC NRS department

function sail_main_calc, l, ala, hot, to, ts, psi, skyl, r_soil, refl, trans, $
				ZD = zd, ZS = zs, TSS = tss, TSD = tsd, TDD = tdd, RS1 = rs1, RDD = rdd

	; Bodenreflexion
	rs1 = r_soil
	rs2 = rs1
	rs3 = rs1

	; Umwandlung der Grad-Werte in Radian f?r SAIL-Modell
	ts = ts * !pi / 180
	to = to * !pi / 180
	psi = psi * !pi / 180

	;------ Beginn des SAIL-Modells   ------------
	;-------------------initialisation des valeurs

	a = 0
	sig = 0
	ks = 0
	ko = 0
	s = 0
	ss = 0
	u = 0
	v = 0
	wo = 0
	w = 0
	rtp = (refl + trans) * 0.5
	rtm = (refl - trans) * 0.5
	tgs = tan(ts)
	tgo = tan(to)
	cos_psi = cos(psi)
	dso = sqrt(abs(tgs ^ 2 + tgo ^ 2 - 2 * tgs * tgo * cos_psi))
	alf = 1e6
	if hot gt 0 then begin
		alf = dso / hot
	endif
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	; calcul de la distribution des inclinaison foliaires

	excent = exp(-1.6184e-5 * long(ala) ^ 3 + 2.1145e-3 * ala ^ 2 - 1.2390e-1 * ala + 3.2491)
	tlref = [5, 15, 25, 35, 45, 55, 64, 72, 77, 79, 81, 83, 85, 87, 89]
	alpha1 = [10, 20, 30, 40, 50, 60, 68, 76, 78, 80, 82, 84, 86, 88, 90]
	alpha2 = [0, alpha1[0:13]]
	ff = dblarr(15)

	; -----------calcul des frequences relatives

	tlref = transpose(tlref) * !pi / 180
	alpha1 = transpose(alpha1) * !pi / 180
	alpha2 = transpose(alpha2) * !pi / 180
	x1 = excent / sqrt(1 + excent ^ 2 * tan(alpha1) ^ 2)
	x2 = excent / sqrt(1 + excent ^ 2 * tan(alpha2) ^ 2)
	if excent eq 1 then begin
		ff = abs(cos(alpha1) - cos(alpha2))
	endif else begin
		a1 = excent / sqrt(abs(1 - excent ^ 2))
		a12 = a1 ^ 2
		x12 = x1 ^ 2
		x22 = x2 ^ 2
		a1px1 = sqrt(a12 + x12)
		a1px2 = sqrt(a12 + x22)
		a1mx1 = sqrt(a12 - x12)
		a1mx2 = sqrt(a12 - x22)
		if excent gt 1 then begin
			ff = x1 * a1px1 + a12 * alog(x1 + a1px1)
			ff = abs(ff - (x2 * a1px2 + a12 * alog(x2 + a1px2)))
		endif else begin
			ff = x1 * a1mx1 + a12* asin(x1 / a1)
			ff = abs(ff - (x2 * a1mx2 + a12 * asin(x2 / a1)))
		endelse
	endelse
	ff = ff / total(ff)

	;----------boucle sur les classes d'angle-------------

	for i = 0, 14 do begin
		tl = tlref(i)
		snl = sin(tl)
		sn2l = snl ^ 2
		csl = cos(tl)
		cs2l = csl ^ 2
		tgl = tan(tl)
		; calcul de betas, beta0, beta1, beta2, beta3
		bs = !pi
		bo = !pi
		if ((tl + ts) gt !pi / 2) then begin
			bs = acos(-1 / (tgs * tgl))
		endif
		if ((tl + to) gt !pi / 2) then begin
			bo = acos(-1 / (tgo * tgl))
		endif
		bt1 = abs(bs - bo)
		bt2 = 2 * !pi - bs - bo
		if psi lt bt1 then begin
		   b1 = psi
		   b2 = bt1
		   b3 = bt2
		endif else if psi gt bt2 then begin
		   b1 = bt1
		   b2 = bt2
		   b3 = psi
		endif else begin
		   b1 = bt1
		   b2 = psi
		   b3 = bt2
		endelse
		; calcul des coefficients de diffusion
		fl = ff[i] * l
		a = a + (1 - rtp + rtm * cs2l) * fl
		sig = sig + (rtp + rtm * cs2l) * fl
		sks = ((bs - !pi * 0.5) * csl + sin(bs) * tgs * snl) * 2 / !pi
		sko = ((bo - !pi * 0.5) * csl + sin(bo) * tgo * snl) * 2 / !pi
		ks = ks + sks * fl
		ko = ko + sko * fl
		s = s + (rtp * sks + rtm * cs2l) * fl
		ss = ss + (rtp * sks + rtm * cs2l) * fl
		u = u + (rtp * sko - rtm * cs2l) * fl
		v = v + (rtp * sko + rtm * cs2l) * fl
		tsin = sn2l * tgs * tgo / 2
		t1 = cs2l + tsin * cos_psi
		t2 = 0
		if b2 gt 0 then begin
			t3 = tsin * 2
			if (bs eq !pi) or (bo eq !pi) then begin
				t3 = cs2l / (cos(bs) * cos(bo))
			endif
			t2 = -b2 * t1 + sin(b2) * (t3 + cos(b1) * cos(b3) * tsin)
		endif
		w = w + (refl * t1 + 2 * rtp * t2 / !pi) * fl
	endfor

	; calcul des variables intermediaires
	m = sqrt(a ^ 2 - sig ^ 2)
	h1 = (a + m) / sig
	h2 = 1 / h1
	cks = ks ^ 2 - m ^ 2
	cko = ko ^ 2 - m ^ 2
	co = (v * (ko - a) - u * sig) / cko
	cs = (ss * (ks - a) - s * sig) / cks
	doo = (-u * (ko + a) - v * sig) / cko
	ds = (-s * (ks + a) - ss * sig) / cks
	ho = (s * co + ss * doo) / (ko + ks)
	; calcul des reflectances et transmittances d'une strate.
	tss = exp(-ks)
	too = exp(-ko)
	g = h1 * exp(m) - h2 * exp(-m)
	rdd = (exp(m) - exp(-m)) / g
	tdd = (h1 - h2) / g
	rsd = cs * (1 - tss * tdd) - ds * rdd
	tsd = ds * (tss - tdd) - cs * tss * rdd
	rdo = co * (1 - too * tdd) - doo * rdd
	tdo = doo * (too - tdd) - co * too * rdd
	rsod = ho * (1 - tss * too) - co * tsd * too - doo * rsd
	; calcul du terme hot-spot
	sumint = 0
	if alf eq 0 then begin
	   tsstoo = tss
	   sumint = (1 - tss) / ks
	endif else begin
		fhot = sqrt(ko * ks)
		x1 = 0
		y1 = 0
		f1 = 1
		fint = (1 - exp(-alf)) * 0.05
		for istep = 1, 20 do begin
			if istep lt 20 then begin
				x2 = -1.0 * alog(1 - istep * fint) / alf
			endif else begin
				x2 = 1
			endelse
			y2 = -(ko + ks) * x2 + fhot * (1 - exp(-alf * x2)) / alf
			f2 = exp(y2)
			sumint = sumint + (f2 - f1) * (x2 - x1) / (y2 - y1)
			x1 = x2
			y1 = y2
			f1 = f2
		endfor
	    tsstoo = f1
	endelse
	rsos = w * sumint
	rso = rsos + rsod
	; calcul des reflectances hemispheriques-directionnelles (zd)
	; et bidirectionnelle (zs)
	xo = 1 - rs3 * rdd
	zd = rdo + tdd * (rs3 * tdo + rs2 * too) / xo
	zs = rso + tsstoo * rs1 + ((tss * rs2 + tsd * rs3) * tdo + (tsd + tss * rs2 * rdd) * rs2 * too) / xo

	return, 0
end

