; _______________________________________________________________________

; prospect.pro
; version 3.01 (5 May 1998)
; subroutines required: s13aaf.pro, and tav.pro
; Translated from Matlab to IDL feb 2008, ITC NRS department
; _______________________________________________________________________

; Plant leaf reflectance and transmittance are calculated from 400 nm to
; 2500 nm with the following parameters:

;       - NN   = leaf structure parameter
;       - Cab = chlorophyll a+b content in �g/cm�
;       - Cw  = equivalent water thickness in g/cm� or cm
;       - Cm  = dry matter content in g/cm�

; Here are some examples observed during the LOPEX'93 experiment on
; fresh (F) and dry (D) leaves :

; ---------------------------------------------
;                NN     Cab     Cw        Cm
; ---------------------------------------------
; min          1.000, 0.0, 0.004000, 0.001900
; max          3.000, 100.0, 0.040000, 0.016500
; corn (F)     1.518, 58.0, 0.013100, 0.003662
; rice (F)     2.275, 23.7, 0.007500, 0.005811
; clover (F)   1.875, 46.7, 0.010000, 0.003014
; laurel (F)   2.660, 74.1, 0.019900, 0.013520
; ---------------------------------------------
; min          1.500, 0.0, 0.000063, 0.0019
; max          3.600, 100.0, 0.000900, 0.0165
; bamboo (D)   2.698, 70.8, 0.000117, 0.009327
; lettuce (D)  2.107, 35.2, 0.000244, 0.002250
; walnut (D)   2.656, 62.8, 0.000263, 0.006573
; chestnut (D) 1.826, 47.7, 0.000307, 0.004305
; ---------------------------------------------

; prospect(NN,Cab,Cw,Cm)
; _______________________________________________________________________

function prospect, NN, Cab, Cw, Cm, data

; ***********************************************************************
; Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G.,
; Hosgood B. (1996), Estimating leaf biochemistry using the PROSPECT
; leaf optical properties model, Remote Sens. Environ., 56:194-202.
; Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical
; properties spectra, Remote Sens. Environ., 34:75-91.
; ***********************************************************************

;  data = nrs_prospect_data()

	l = data[0, *] ; l = first column
	n = data[1, *] ; n = second column
	k = (Cab * data[3, *] $
		+ Cw * data[4, *] $
		+ Cm * data[5, *]) / NN $
		+ data[2, *]

; ***********************************************************************
; reflectance and transmittance of one layer
; ***********************************************************************
; Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969),
; Interaction of isotropic ligth with a compact plant leaf, J. Opt.
; Soc. Am., 59(10):1376-1379.
; ***********************************************************************

	sel = where(k lt 0, sel_count)
	if sel_count ne 0 then k[sel] = 1
	sel = where(k ge 0, sel_count)
	if sel_count ne 0 then k = (1 - k) * exp(-k) + (k ^ 2) * s13aaf(k)

	t1 = tav(90, n)
	t2 = tav(60, n)
	x1 = 1 - t1
	x2 = (t1 ^ 2) * (k ^ 2) * (n ^ 2 - t1)
	x3 = (t1 ^ 2) * k * n ^ 2
	x4 = n ^ 4 - (k ^2) * (n ^ 2 - t1) ^ 2
	x5 = t2 / t1
	x6 = x5 * (t1 - 1) + 1 - t2

	r = x1 + x2 / x4
	t = x3 / x4
	ra = x5 * r + x6
	ta = x5 * t

; ***********************************************************************
; reflectance and transmittance of NN layers
; ***********************************************************************
; Stokes G.G. (1862), On the intensity of the light reflected from
; or transmitted through a pile of plates, Proc. Roy. Soc. Lond.,
; 11:545-556.
; ***********************************************************************

	delta = (t ^ 2 - r ^ 2 - 1) ^ 2 - 4 * r ^ 2
	alfa = (1 + r ^ 2 - t ^ 2 + sqrt(delta)) / (2 * r)
	beta = (1 + r ^ 2 - t ^ 2 - sqrt(delta)) / (2 * r)
	va =   (1 + r ^ 2 - t ^ 2 + sqrt(delta)) / (2 * r)
	vb = sqrt(beta * (alfa - r) / (alfa * (beta - r)))
	s11 = ra * (va * (vb ^ (NN-1)) - (1 / va) * vb ^ (-(NN - 1)))
	s12 = (ta * t - ra * r) * (vb ^ (NN-1) - vb ^ (-(NN - 1)))
	s2 = ta * (va - 1 / va)
	s3 = va * vb ^ (NN - 1) - (1 / va) * vb ^ (-(NN - 1)) -r * (vb ^ (NN - 1) - vb ^ (-(NN - 1)))

	LRT = [l, (s11 + s12) /s3, s2 / s3]

	return, LRT
end
