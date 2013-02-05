; SAIL + PROSPECT + SOIL-Model
; Simulation of spectral reflection from 404 to 2500 nm as a function of
; biophysical parameter.
; The underlying SAIL version originates from F.BARET (III.1993)
; in "SAIL1(l,ala,hot,refl,tran,rs1,rs2,rs3,to,ts,psi,skyl)"
;
; The PROSPECT Model writes as: "refl/tran = LEAF(cab,cw,vai)
; The spectral bidirectional (rs1), hemispherical-directional (rs2) and
; bihemispherical soilreflection (rs3) are simulated by the SOILSPEC-program
; with "SOIL(ts,to,psi)".
;
;   All this gives:
; ----- sail_inf(l, ala, hot, to, ts, psi, skyl, r_soil, refl, trans) ----------
; l     : Leaf Area Index (LAI)
; ala   : Average leaf inclination angle
; hot   : parametre hotspot: rapport entre diametre feuille et hauteur strate
; to    : Zenith angle of the observation
; ts    : Solar zenith angle
; psi   : Azimutal angle between observation and sun
; skyl  : Diffuser Strahlungsanteil
; r_soil:
; refl  : Leaf reflectance
; trans : Leaf transmittance
;
; For infinite reflection (rc)
; Clement Atzberger im November 1995
; Translated from Matlab to IDL feb 2008, W. Nieuwenhuis, ITC NRS department

function sail_inf, l, ala, hot, to, ts, psi, skyl, r_soil, refl, trans

	m_to = to
	m_ts = ts
	m_psi = psi
	calc = sail_main_calc(l, ala, hot, m_to, m_ts, m_psi, skyl, r_soil, refl, trans, $
			ZD = zd, ZS = zs)

	; calculation of the bidirectionnal reflectance
	rsail = zd * skyl + (1 - skyl) * zs

	return, rsail
end
