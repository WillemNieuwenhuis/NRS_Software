; Translated from Matlab to IDL feb 2008, ITC NRS department

function inform_prospect_single

	r_soil = default_soil_spectrum()

	N = 2
	Cab = 40
	Cw = 0.025
	Cm = 0.001

	hot = 0.02
	scale = 0.5
	lai = 5
	lai_u = 1
	sd = 500
	h = 20
	cd = 7
	ala = 55

	teta_s = 30
	teta_o = 0
	phi = 0
	skyl = 0.1

; return value contains:
;	[r_forest,R_SOIL,r_understorey,r_c_inf,co,C,G,r_leaf,t_leaf,t_s,t_o]

	res = inform_prospect([N, Cab, Cw, Cm], $
				[hot, scale, lai, lai_u, sd, h, cd, ala], $
				teta_s, teta_o, phi, skyl, r_soil, $
				R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
				R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
				T_S = t_s, T_O = t_o $
			  )
	return, 0
end