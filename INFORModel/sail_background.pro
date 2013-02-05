; SSP1.M SAIL+PROSPECT+SOIL-Modell zur Simulation der spektralen
; Reflektion von 404 bis 2500 nm als Funktion biophysikalischer Parameter
; Die dahinterstehende SAIL Version stammt von F.BARET (III.1993)
; mit SAIL1(l,ala,hot,refl,tran,rs1,rs2,rs3,to,ts,psi,skyl)
; Das PROSPECT Modell schreibt sich refl/tran = LEAF(cab,cw,vai)
; Insgesamt ergibt sich:
; ----------- SAILSPEC(l,ala,hot,cab,cw,vai,to,ts,psi,skyl,SOILREFL) --------------
; l     : LAI
; ala   : angle moyen d'inclinaison des feuilles
; hot   : parametre hotspot: rapport entre diametre feuille et hauteur strate
; refl  : reflectance des feuilles   tran: transmittance des feuilles
; cab   : Chlorphyllkonzentration
; cw    : Wassergehalt der Bl�tter
; vai   : Void Area Index
; to    : angle zenithal d'observation
; ts    : angle zenithal solaire
; psi   : angle azimutal entre observation et soleil
; skyl  : diffuser Strahlungsanteil
;
; Clement Atzberger im November 1995
; Translated from Matlab to IDL feb 2008, ITC NRS department

function sail_background, l, ala, hot, N, Cab, Cw, Cm, to, ts, psi, skyl, r_soil
	; ------ Beginn des PROSPECT-Modells ------------ %
	data = nrs_prospect_data()
	lf = prospect(N, Cab, Cw, Cm, data)
	refl = lf[1, *]
	trans = lf[2, *]
	; ------ Ende des PROSPECT-Modells --------------- %

	rsail = sail_inf(l, ala, hot, to, ts, psi, skyl, r_soil, refl, trans)

	return, rsail
end
