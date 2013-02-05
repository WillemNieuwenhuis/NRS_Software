; ----------- TRANS_S(l,ala,hot,cab,cw,vai,ts,skyl) --------------
; l     : LAI
; ala   : angle moyen d'inclinaison des feuilles
; hot   : parametre hotspot: rapport entre diametre feuille et hauteur strate
; refl  : reflectance des feuilles
; trans : transmittance des feuilles
; cab   : Chlorphyllkonzentration
; cw    : Wassergehalt der Bl„tter
; vai   : Void Area Index
; to    : angle zenithal d'observation
; ts    : angle zenithal solaire
; psi   : angle azimutal entre observation et soleil
; skyl  : diffuser Strahlungsanteil
;
; Clement Atzberger im November 1995
; Translated from Matlab to IDL feb 2008, ITC NRS department

function sail_t_so, l, ala, hot, ts, skyl, r_soil, to, psi, refl, trans

	m_to = to
	m_ts = ts
	m_psi = psi
	calc = sail_main_calc(l, ala, hot, m_to, m_ts, m_psi, skyl, r_soil, refl, trans, $
			TSS = tss, TSD = tsd, TDD = tdd, RS1 = rs1, RDD = rdd)

	; calcul de la reflectance bidirectionnelle
	_ed = skyl
	_es = 1 - skyl

	_tsd = tss + (tsd + tss * rs1 * rdd) / (1 - rs1 * rdd)
	_tdd = tdd / (1 - rs1 * rdd)

	trans_hemi = _tsd * (1 - skyl) + _tdd * skyl

	return, trans_hemi
end
