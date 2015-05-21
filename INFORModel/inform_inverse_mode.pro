;+
; :description:
;   determine the optimal parameter set for the inform prospect model
;   using inverse mode.
;   The inverse mode iterates through the parameters in par_array (each
;   parameter a number of "steps" times between the min/max as specified
;   by the user; all other parameters remain fixed (also specified by user).
;
;   For each parameter set the model is run and the output spectrum is
;   matched against the reference spectrum, with lsqfit. The parameter set
;   with the lowest error in the lsqfit is returned as the optimal set.
;
; :Params:
;	  reflectanceType: in, required
;	    Indicate which of the reflection spectra to use in the lsqfit
;	  steps: in, required
;	    The number of steps in which the 'active' parameters
;     will be evaluated
;	  refspec: in, required
;     The reference spectrum to match the model results against
;     It has two columns: wavelength and reflectance
;	  par_array: in, required
;     The array with active parameters during the iteration
;	  fix_array: in, required
;     The list with all parameters and their current value (user specified)
;	  r_soil: in, required
;	    The soil spectrum needed for the model
;
; :Returns:
;	  Parameter array with optimal parameters and the error belonging to this set
;	  The error is the last element in the array.
;-
function inform_inverse_mode, reflectanceType, steps, refspec, par_array, fix_array, r_soil
	optimal_params = 0

	index = 0
	N      = fix_array[index++]
	Cab    = fix_array[index++]
	Cw     = fix_array[index++]
	Cm     = fix_array[index++]
	hot    = fix_array[index++]
	scale  = fix_array[index++]
	lai    = fix_array[index++]
	lai_u  = fix_array[index++]
	sd     = fix_array[index++]
	h      = fix_array[index++]
	cd     = fix_array[index++]
	ala    = fix_array[index++]
	teta_s = fix_array[index++]
	teta_o = fix_array[index++]
	phi    = fix_array[index++]
	skyl   = fix_array[index++]

	; calculate all active-parameter permutations
	activeParams = n_elements(par_array)
	permutationCount = steps ^ activeParams

	if permutationCount ge 250 then begin
		; initialise tranquilizer
		progressBar = Obj_New("PROGRESSBAR", /nocancel, ysize = 15, title = "Sensitivity (Inverse mode)")
		progressBar -> Start

		catch, stat
		if stat ne 0 then begin
			; cleanup if some error
			progressBar -> Destroy
			catch, /cancel

			ans = dialog_message(!error_state.msg, title = 'Error', /error)
			return, optimal_params
		endif

		progressBar -> Update, 0.0, text = 'Initializing permutations'
	endif

	perms = fltarr(activeParams, permutationCount)
	for p = 0, activeParams - 1 do begin
		parm = par_array[p]
		minval = parm.min_val
		maxval = parm.max_val
		par_step = (maxval - minval) / (steps - 1)
		fac = 10 ^ p
		for pi = 0, permutationCount - 1 do begin
			mulfac = ((pi / fac) mod steps)
			perms[p, pi] = minval + par_step * mulfac
		end
	endfor

	parms1 = [N, Cab, Cw, Cm]
	parms2 = [hot, scale, lai, lai_u, sd, h, cd, ala]

	reflect = 0		; Forest
	if reflectanceType eq 'Forest' then reflect = 0
	if reflectanceType eq 'Understorey' then reflect = 1
	if reflectanceType eq 'Infinite canopy' then reflect = 2
	if reflectanceType eq 'Leaf' then reflect = 3

	; generate the list of wavelengths
	wv_index = indgen(421) * 5 + 400

	; do the calculation iterations
	optimal_params = [parms1, parms2, teta_s, teta_o, phi, skyl]
	low_err = 1.0e38

;	logfile='d:\temp\prospect.csv'
;	openw, lun, logfile, /get_lun

	for p = 0, permutationCount - 1 do begin
		if permutationCount ge 250 then $
			progressBar -> Update, 100.0 * p / (permutationCount - 1) $
				, text = 'Processing permutation ' + string(p, format = '(i0)') + ' of ' + string(permutationCount, format = '(i0)')

		setParameters, p, par_array, perms, parms1, parms2, teta_s, teta_o, phi, skyl
		test_params = [parms1, parms2, teta_s, teta_o, phi, skyl]

;		format = '(' + string(n_elements(test_params), format='(i0)') + '(a,","))'
;		logstr = string(test_params, format = format)

		; run the model with the new parameters
		res = inform_prospect(parms1, $
				parms2, $
				teta_s, teta_o, phi, skyl, r_soil, $
				R_FOREST = r_forest, R_SOIL_SCALE = r_soil_scale, R_UNDERSTORY = r_understorey, $
				R_C_INF = r_c_inf, CO = co, CC = C, G = G, R_LEAF = r_leaf, T_LEAF = t_leaf, $
				T_S = t_s, T_O = t_o $
			  )

		; now do the least squares fit
		case reflect of
			0: ref_refl = r_forest
			1: ref_refl = r_understorey
			2: ref_refl = r_c_inf
			3: ref_refl = r_leaf
		endcase

		ref_index = refspec[0, *]
		ref = refspec[1, *]

		if n_elements(wv_index) eq n_elements(ref_index) then begin
			sqr_err = total((ref_refl - ref) ^ 2)
		endif else begin
			; nearest neighbor matching
			sqr_err = 0
			for ri = 0, n_elements(ref_index) - 1 do begin
				wv_i = round((ref_index[ri] - 400.0) / 5)
				sqr_err += (ref_refl[ri] - ref[ri]) ^ 2
			endfor
		endelse

;		logstr += string(sqr_err, format = '(f0.5)')

		if sqr_err lt low_err then begin
			optimal_params = test_params
			low_err = sqr_err
		endif

;		printf, lun, logstr

	endfor

;	close, lun
;	free_lun, lun

	if permutationCount ge 250 then $
		progressBar -> Destroy

	return, [optimal_params, low_err]
end

; Update the parameter set for a next iteration. Only the active
; parameters are updated; the fixed parameters keep their values
; Parameters:
;	iter:				The index of the next iteration
;	activeParms:		The list of parameters that are active in the iteration
;	perms:				The permutation list of values for the active parameters
;	parms1:				parameters [N, Cab, Cw, Cm]
;	parms2:				parameters [hot, scale, lai, lai_u, sd, h, cd, ala]
;	teta_s:				teta_s
;	teta_o:				teta_o
;	phi:				phi
;	skyl:				skyl
pro setParameters, iter, activeParms, perms, parms1, parms2, teta_s, teta_o, phi, skyl
	newvals = perms[*, iter]
	for pv = 0, n_elements(newvals) - 1 do begin
		parmval = activeParms[pv]
		pi = getIndexOfParam(parmval.name)
		case pi of
			0:	parms1[0]	= newvals[pv]	; N
			1:	parms1[1]	= newvals[pv]	; Cab
			2:	parms1[2]	= newvals[pv]	; Cw
			3:	parms1[3]	= newvals[pv]	; Cm
			4:	parms2[0]	= newvals[pv]	; hot
			5:	parms2[1]	= newvals[pv]	; scale
			6:	parms2[2]	= newvals[pv]	; lai
			7:	parms2[3]	= newvals[pv]	; lai_u
			8:	parms2[4]	= newvals[pv]	; sd
			9:	parms2[5]	= newvals[pv]	; h
			10:	parms2[6]	= newvals[pv]	; cd
			11:	parms2[7]	= newvals[pv]	; ala
			12:	teta_s		= newvals[pv]	; teta_s
			13:	teta_o		= newvals[pv]	; teta_o
			14:	phi			= newvals[pv]	; phi
			15:	skyl		= newvals[pv]	; skyl
		endcase
	endfor
end
