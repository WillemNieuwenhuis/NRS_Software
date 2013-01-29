; docformat = 'rst'
;+
; :Description:
; Determine the crystals in a wavelet coefficient matrix
; The matrix is square with 'side' x 'side' coefficients
; The number of 'levels' determines the crystals
; The procedure generates for each crystal:
; <ul>
;	  <li>the boundary in the coefficient matrix
;	  <li>the name of the crystal
;	</ul>
; Crystal names follow the s-plus convention:
; <ul>
;	 <li>Si-Di:	horizontal coefficients
;	 <li>Di-Si:	vertical coefficients
;	 <li>Di-Di:	diagonal coefficients
;	 <li>Si-Si:	smooth coefficients (only at the deepest level)
; </ul>
;
; :Note:
;   the actual matrix is not required here, because only the
;   metadata of the crystals is generated
; :Params:
;	  side: in, required
;     the size of the both dimensions of the matrix
;	  levels: in, required
;	    The number of levels for which to generate the crystal info
;
;	  crystals: out, required
;	  	a list of boundary records for each crystal (left, top, right, bottom)
;	  crystalNames: out, required
;	   a list with the crystal names (same order as 'crystals')
;-
pro determineCrystals, side, levels, crystals, crystalNames
	index = 0
	crystals = intarr(4, 3 * levels + 1)
	crystalNames = strarr(3 * levels + 1)
	len = side
	for lv = 1, levels do begin
		si = 's' + string(lv, format='(i-0)')
		di = 'd' + string(lv, format='(i-0)')
		len = len / 2
		; order of crystals in the array = [left, top, right, bottom]
		; get si-di
		crystals[0,  index]    = [0, len, len - 1, len * 2 - 1]
		crystalNames[index]    = si + '-' + di
		; get di-si
		crystals[0,  index + 1] = [len, 0, len * 2 - 1, len - 1]
		crystalNames[index + 1] = di + '-' + si
		; get di-di
		crystals[0,  index + 2] = [len, len, len * 2 - 1, len * 2 - 1]
		crystalNames[index + 2] = di + '-' + di
		index = index + 3
	end
	; only need to add si-si for i = levels + 1
	crystals[0,  index] = [0, 0, len - 1, len - 1]
	crystalNames[index] = 's' + string(levels, format='(i-0)') $
							  + '-s' + string(levels, format='(i-0)')
end

; Determine the crystal names in a wavelet coefficient matrix
; The matrix is square with 'side' x 'side' coefficients
; The number of 'levels' determines the crystals
; The procedure generates for each crystal:
;	- the name of the crystal
; Crystal names follow the s-plus convention:
;	- Si-Di:	horizontal coefficients
;	- Di-Si:	vertical coefficients
;	- Di-Di:	diagonal coefficients
;	- Si-Si:	smooth coefficients (all levels)
;
; Note: the actual matrix is not required here, because only the
;       metadata of the crystals is generated
; Parameters:
;	side[input]:	the size of the both dimensions of the matrix
;	levels[input]:	The number of levels for which to generate the crystal info
;
;	crystalNames[output]: a list with the crystal names (same order as 'crystals')
pro determineAllCrystalNames, side, levels, crystalNames
	index = 0
	crystalNames = strarr(4 * levels)
	len = side
	for lv = 1, levels do begin
		si = 's' + string(lv, format='(i-0)')
		di = 'd' + string(lv, format='(i-0)')
		len = len / 2
		; order of crystals in the array = [left, top, right, bottom]
		; get si-di
		crystalNames[index]    = si + '-' + di
		; get di-si
		crystalNames[index + 1] = di + '-' + si
		; get di-di
		crystalNames[index + 2] = di + '-' + di
		; get si-si
		crystalNames[index + 3] = 's' + string(lv, format='(i-0)') $
							  + '-s' + string(lv, format='(i-0)')
		index = index + 4
	end
end
; calculate the spectrum summary for the wavelet decomposition
; parameters:
;	coef[input]		the image containing the wavelet decomposition
;	levels[input]	the decomposition level (normally the same as maxLevel)
;	maxLevel[input]	the maximum decomposition level
;	crystals[output]	array with all extracted crystals
;	names[output]		array with names of all extracted crystals; uses
;						same index as 'crystals' array
;	smyArray[output]	array containing the total summary
;   smySubTotals[out]   array containing summary information for each level
pro calcSummary, coef, levels, maxLevel, crystals, names, smyArray, smySubTotals
	maxLevel = min([maxLevel, levels])
	; get a list of the crystal boundaries
	len = n_elements(coef[*, 0])
	determineCrystals, len, maxLevel, crystals, names

	; for each crystal calculate summary and write to disk
	crystCount = 3 * maxLevel
	smyArray = fltarr(8, crystCount + 1) ; 8 is the #columns of the summary result
	for cc = 0, crystCount do begin
		bounds = crystals[*, cc]
		left = bounds[0]
		top = bounds[1]
		right = bounds[2]
		bottom = bounds[3]
		name = names[cc]
		smy = summary(coef[left:right,top:bottom])
		smyArray[*, cc] = smy
	endfor

	energySum = total(smyArray[7, *])

	; calculate the relative energy for all crystals
	for cc = 0, crystCount do begin
		val = smyArray[7, cc]
		smyArray[7, cc] = val / energySum
	endfor

	; calculate subtotals for each decomposition level
	smySubTotals = fltarr(8, maxLevel) ; 8 is the #columns of the summary result
	cc = 0
	lvl = 0
	while cc lt crystCount - 1 do begin
		cnt = 0
		while cnt lt 3 do begin
			bounds = crystals[*, cc + cnt]
			left = bounds[0]
			top = bounds[1]
			right = bounds[2]
			bottom = bounds[3]
			if cnt eq 0 then begin
				sub = coef[left:right,top:bottom]
			endif else begin
				sub = [sub, coef[left:right,top:bottom]]
			endelse
			cnt +=1
		endwhile
		smy = summary(sub)
		smySubTotals[*, lvl] = smy
		lvl += 1
		cc += 3
	endwhile
	; calculate the relative energy for all sub-totals
	for cc = 0, maxLevel - 1 do begin
		val = smySubTotals[7, cc]
		smySubTotals[7, cc] = val / energySum
	endfor

end

;+
; :Description:
; calculate the spectrum summary for the wavelet decomposition
; Alternative approach to massively reduce memory usage
; :params:
;	  coef: in, required
;	    the image containing the wavelet decomposition
;	  levels: in, required
;	    the decomposition level (normally the same as maxLevel)
;	  maxLevel: in, required
;	    the maximum decomposition level
;	  crystals: out, required
;	    array with all extracted crystals
;	  names: out,required
;	    array with names of all extracted crystals; uses
;						same index as 'crystals' array
;	  smyArray: out, required
;	    array containing the total summary
;   smySubTotals: out, required
;     array containing summary information for each level
;-
pro calcSummary_alt, coef, levels, maxLevel, crystals, names, smyArray, smySubTotals
	maxLevel = min([maxLevel, levels])
	; get a list of the crystal boundaries
	len = sqrt(n_elements(coef))
	determineCrystals, len, maxLevel, crystals, names

  energy_col = 7
  sum_cols = 8
  
	; for each crystal calculate summary and write to disk
	crystCount = 3 * maxLevel
	smyArray = fltarr(sum_cols, crystCount + 1) ; sum_cols is the #columns of the summary result

	; initialise tranquilizer
	envi_report_init,["Calculating wavelet statistics",$
                      "This can take a few minutes"],$
						base = tranq, title = "progress"
	tranqrange = crystCount * 2
	envi_report_inc, tranq, tranqrange

	for cc = 0, crystCount do begin
		envi_report_stat, tranq, cc, crystCount * 2, cancel = cancel
		bounds = crystals[*, cc]
		left = bounds[0]
		top = bounds[1]
		right = bounds[2]
		bottom = bounds[3]
		name = names[cc]
		range = [left, top, right, bottom]
		smy = Summary_mem(coef, range)
		smyArray[*, cc] = smy
	endfor

	energySum = total(smyArray[energy_col, *])
	; calculate the relative energy for all crystals
	for cc = 0, crystCount do begin
		val = smyArray[energy_col, cc]
		smyArray[energy_col, cc] = val / energySum
	endfor
	
	; calculate subtotals for each decomposition level
	smySubTotals = fltarr(sum_cols, maxLevel) ; sum_cols is the #columns of the summary result
	cc = 0
	lvl = 0
	while cc lt crystCount - 1 do begin
		envi_report_stat, tranq, cc + crystCount, crystCount * 2, cancel = cancel
		cnt = 0
		ranges = lonarr(4, 3)
		while cnt lt 3 do begin
			bounds = crystals[*, cc + cnt]
			ranges[0, cnt] = bounds[0]
			ranges[1, cnt] = bounds[1]
			ranges[2, cnt] = bounds[2]
			ranges[3, cnt] = bounds[3]
			cnt +=1
		endwhile

		smy = summary_mem(coef, ranges)
		smySubTotals[*, lvl] = smy
		lvl += 1
		cc += 3
	endwhile

	envi_report_init, base=tranq, /finish

	; calculate the relative energy for all sub-totals
	for cc = 0, maxLevel - 1 do begin
		val = smySubTotals[energy_col, cc]
		smySubTotals[energy_col, cc] = val / energySum
	endfor

end

;+
; :Description:
;   Calculate the spectrum summary for the wavelet decomposition
;   This version removes outliers from the coefficients first
; :params:
;   coef: in, required
;     the image containing the wavelet decomposition
;   levels: in, required
;     the decomposition level (normally the same as maxLevel)
;   maxLevel: in, required
;     the maximum decomposition level
;   crystals: out, required
;     array with all extracted crystals
;   names: out,required
;     array with names of all extracted crystals; uses
;           same index as 'crystals' array
;   smyArray: out, required
;     array containing the total summary
;   smySubTotals: out, required
;     array containing summary information for each level
;-
pro calcSummary_Q, coef, levels, maxLevel, crystals, names, smyArray, smySubTotals
  maxLevel = min([maxLevel, levels])
  ; get a list of the crystal boundaries
  len = sqrt(n_elements(coef))
  determineCrystals, len, maxLevel, crystals, names

  energy_col = 7
  sum_cols = 10 ; extra room for Q1 and Q3 quartiles thresholds
  
  ; for each crystal calculate summary and write to disk
  crystCount = 3 * maxLevel
  smyArray = fltarr(sum_cols, crystCount + 1) ; sum_cols is the #columns of the summary result

  ; initialise tranquilizer
  envi_report_init,["Calculating wavelet statistics (without outliers)",$
                      "This can take a few minutes"],$
            base = tranq, title = "progress"
  tranqrange = crystCount * 2
  envi_report_inc, tranq, tranqrange

  for cc = 0, crystCount do begin
    envi_report_stat, tranq, cc, crystCount * 2, cancel = cancel
    bounds = crystals[*, cc]
    left = bounds[0]
    top = bounds[1]
    right = bounds[2]
    bottom = bounds[3]
    name = names[cc]
    range = [left, top, right, bottom]
    smy = Summary_Q(coef, range)
    smyArray[*, cc] = smy
  endfor

  energySum = total(smyArray[energy_col, *])
  ; calculate the relative energy for all crystals
  for cc = 0, crystCount do begin
    val = smyArray[energy_col, cc]
    smyArray[energy_col, cc] = val / energySum
  endfor
  
  ; calculate subtotals for each decomposition level
  smySubTotals = fltarr(sum_cols, maxLevel) ; sum_cols is the #columns of the summary result
  cc = 0
  lvl = 0
  while cc lt crystCount - 1 do begin
    envi_report_stat, tranq, cc + crystCount, crystCount * 2, cancel = cancel
    cnt = 0
    ranges = lonarr(4, 3)
    while cnt lt 3 do begin
      bounds = crystals[*, cc + cnt]
      ranges[0, cnt] = bounds[0]
      ranges[1, cnt] = bounds[1]
      ranges[2, cnt] = bounds[2]
      ranges[3, cnt] = bounds[3]
      cnt +=1
    endwhile

    smy = summary_Q(coef, ranges)
    smySubTotals[*, lvl] = smy
    lvl += 1
    cc += 3
  endwhile

  envi_report_init, base=tranq, /finish

  ; calculate the relative energy for all sub-totals
  for cc = 0, maxLevel - 1 do begin
    val = smySubTotals[energy_col, cc]
    smySubTotals[energy_col, cc] = val / energySum
  endfor

end