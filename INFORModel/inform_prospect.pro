; INFORM_Prospect - The INvertible FOrest Reflectance Model coupled with PROSPECT

; INFORM (Atzberger, 2000; Schlerf&Atzberger, 2006) simulates the bi-directional reflectance
; of forest stands between 400 and 2500 nm. INFORM is essentially an innovative combination of
; FLIM (Rosema et al., 1992), SAIL (Verhoef, 1984), and PROSPECT (Jacquemoud et al. 1996)

; Atzberger, C. 2000: Development of an invertible forest reflectance model: The INFOR-Model.
; In: Buchroithner (Ed.): A decade of trans-european remote sensing cooperation. Proceedings
; of the 20th EARSeL Symposium Dresden, Germany, 14.-16. June 2000: 39-44.

; Schlerf, M. & Atzberger, C. (2006): Inversion of a forest reflectance model to estimate biophysical
; canopy variables from hyperspectral remote sensing data. Remote Sensing of Environment, 100: 281-294

; Rosema, A., Verhoef, W., Noorbergen, H. 1992: A new forest light interaction model in support of forest
; monitoring. Remote Sensing of Environment, 42: 23-41.

; Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B. (1996): Estimating leaf
; biochemistry using the PROSPECT leaf optical properties model, Remote Sens. Environ., 56:194-202.

; Verhoef, W. 1984: Light scattering by leaf layers with application to canopy reflectance modeling: The
; SAIL model. Remote Sensing of Environment, 16: 125-141.

; Basic version of INFORM: Clement Atzberger, 1999
; INFORM modifications and validation: Martin Schlerf, 2004-2007
; _____________________________________________________________________________________________________________

; SUBROUTINES

; PRO-File                    Function
; inform_prospect_single.pro  Start routine; initializes parameters and starts the INFOR-Model
; inform_prospect.pro         Main INFORM code
; sail_main_calc.pro          Main SAIL-PROSPECT-SOIL compute module, used by "sail_background",
;                               "sail_inf" and "sail_t_so"
; sail_background.pro         SAIL-PROSPECT-SOIL to compute background reflectance
; sail_inf.pro                SAIL-PROSPECT-SOIL to compute infinite crown reflectance
; sail_t_so.pro               SAIL-PROSPECT-SOIL to compute crown transmittance for sun or
;                               observation direction
; prospect.pro                Leaf reflecance model PROSPECT
; s13aaf.pro                  Integral for PROSPECT
; tav.pro                     Refraction index for PROSPECT
;
; Absorption coefficients for PROSPECT moved to the inform_prospect_single.pro module

; _____________________________________________________________________________________________________________

; INPUT VARIABLES

; PARA_PROSPECT: Leaf Input Parameters
; Variable	                         Designation	    Unit	    Default value
; leaf structure parameter              N               -           2
; chlorophyll a+b content in            Cab             �g/cm�      60
; equivalent water thickness            Cw              g/cm�       0.025
; dry matter content                    Cm              g/cm�       0.025

; PARA_INFORM: Canopy Input Parameters
; Variable	                          Designation       Unit	    Default value
; Scale factor for soil reflectance     scale                       1
; Single tree leaf area index           lai             m2 m-2      7
; Leaf area index of understorey	    laiu	        m2 m-2	    0.1
; Stem density                          sd              ha-1        650
; Tree height                           h               m           20
; Crown diameter                        cd              m           4.5
; Average leaf angle of tree canopy	    ala	            deg	        55


; External Input Parameters
; Variable	                          Designation       Unit	    Default value
; Sun zenith angle 	                    teta_s	        deg	        30
; Observation zenith angle 	            teta_o	        deg	        0
; Azimuth angle	                        phi	            deg	        0
; Fraction of diffuse radiation	        skyl	        fraction	0.1

; Other Input Data
; Variable	                          Designation
; r_soil                                Soil spectrum
; _____________________________________________________________________________________________________________

; OUTPUT VARIABLES
; Variable	                          Designation
; Forest reflectance                    r_forest
; Soil reflectance                      r_soil
; Understorey reflectance               r_understorey
; Infinite canopy reflectance           r_c_inf
; Crown closure                         co
; Crown factor                          C
; Ground factor                         G
; Leaf reflectance                      r_leaf
; Leaf transmittance                    t_leaf
; Crown transmittance for teta_s        t_s
; Crown transmittance for teta_o        t_o
; _____________________________________________________________________________________________________________
; Translated from Matlab to IDL feb 2008, ITC NRS department

function inform_prospect, PARA_PROSPECT, PARA_INFORM, teta_o, teta_s, phi, skyl, r_soil, $
		R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
		R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
		T_S = t_s, T_O = t_o

	hot = PARA_INFORM[0]
	scale = PARA_INFORM[1]
	lai = PARA_INFORM[2]
	laiu = PARA_INFORM[3]
	sd = PARA_INFORM[4]
	h = PARA_INFORM[5]
	cd = PARA_INFORM[6]
	ala = PARA_INFORM[7]

	N = PARA_PROSPECT[0]
	Cab = PARA_PROSPECT[1]
	Cw = PARA_PROSPECT[2]
	Cm = PARA_PROSPECT[3]

	; _____________________________________________________________________________________________________________

	; Computing spectral components (SAIL, PROSPECT, LIBERTY models)

	; Scaling of soil spectrum (to account for effects due to shadow and soil moisture)
	r_soil_scale = scale * r_soil

	; Computing of understorey reflectance for dicotyledoneae (ala = 45, N = 2, cab = 50, cw = 0.025, cm = 0.025)
	r_understorey = sail_background(laiu, 45, hot, 2, 50, 0.025, 0.025, teta_o, teta_s, phi, skyl, r_soil_scale)

	; Computing of leaf reflecance and transmittance
  data = nrs_prospect_data()
	rt = prospect(N, Cab, Cw, Cm, data)
	r_leaf = rt[1, *]
	t_leaf = rt[2, *]

	; Computing of infinitive crown reflectance for a very dense forest canopy (LAI = 15)
	r_c_inf = sail_inf(15, ala, hot, teta_o, teta_s, phi, skyl, r_understorey, r_leaf, t_leaf)
	; _____________________________________________________________________________________________________________

	; Ground coverage (FLIM model)

	; Computation of the average horizontal area of a single tree crown in hectare (k) corrected
	; by the factor 'adapt'. 'adapt' was adjusted in such a way, that modelled values of 'co'
	; agreed with measured values of 'co' for a given stand structure (as defined by 'cd' and
	; 'sd'). To compare measured with modelled stands for certain values of canopy lai (lai_c),
	; an empirical relation between 'lai_c' and 'co' was derived from field data at Idarwald site
	; (co = 0.078*lai_c+0.23, n = 40).

	; adapt = 1; % no correction
	; adapt = 0.6
	adapt = 1

	k = adapt * (!pi * (cd / 2) ^ 2) / 10000

	; angles (degree) to angles (radian)
	teta_o = teta_o * !pi / 180
	teta_s = teta_s * !pi / 180
	phi = phi * !pi / 180

	; Observed ground coverage  by crowns (co) under observation zenith angle teta_o
	co = 1 - exp(-k * sd / cos(teta_o))

	; Ground coverage by shadow (cs) under a solar zenith angle teta_s
	cs = 1-exp(-k * sd / cos(teta_s))

	; Geometrical factor (g) depending on the illumination and viewing geometry
	g = sqrt((tan(teta_o)) ^ 2 + (tan(teta_s)) ^ 2 - 2 * tan(teta_o) * tan(teta_s) * cos(phi))

	; Correlation coefficient (p)
	p = exp(-g * h / cd)

	; _____________________________________________________________________________________________________________

	; Ground surface fractions (FLIM model)
	corr_coef = p * sqrt(co * (1 - co) * cs * (1 - cs))

	; Tree crowns with shadowed background (Fcd)
	Fcd = co * cs + corr_coef

	; Tree crowns with sunlit background (Fcs)
	Fcs = co * (1 - cs) - corr_coef

	; Shadowed open space (Fod)
	Fod = (1 - co) * cs - corr_coef

	; Sunlit open space (Fos)
	Fos = (1 - co) * (1 - cs) + corr_coef

	; _____________________________________________________________________________________________________________

	; Crown transmittance (SAIL model)

	; Angles (radian) to angles (degree)
	teta_s = teta_s * 180 / !pi
	teta_o = teta_o * 180 / !pi

	; Crown transmittance in sun direction (t_s)
	t_s = sail_t_so(lai, ala, 0, teta_s, skyl, r_understorey, teta_o, phi, r_leaf, t_leaf)

	; Crown transmittance in observation direction (t_o)
	t_o = sail_t_so(lai, ala, 0, teta_o, skyl, r_understorey, teta_o, phi, r_leaf, t_leaf)

	; _____________________________________________________________________________________________________________

	; Forest reflectance (FLIM model)

	; Ground factor (G), that is ground contribution to scene reflectance
	G = Fcd * t_s * t_o + Fcs * t_o + Fod * t_s + Fos

	; Crown factor (C), that is crown contribution to scene reflectance
	; C = (1-t_s.*t_o)*cs*co;  % Original formula
	CC = Fcd * (1 - t_s * t_o); % Similar to original formula
	; C = Fcd * (1 - t_s * t_o) + Fcs * (1 - t_o); % Modification, rejected by W. Verhoef
	; Der Faktor 'Fcs*(1-t_o)' wurde erg�nzt. Er beschreibt den Beitrag, der von
	; beleuchteten Kronen (sunlit crowns, Fcs) erfolgt abz�glich des Beitrags
	; des darunterliegenden Untergrundes (Fcs*t_o), der schon in G enthalten
	; ist.

	; Forest reflectance
	r_forest = r_c_inf * CC + r_understorey * G

	return, 0
end
