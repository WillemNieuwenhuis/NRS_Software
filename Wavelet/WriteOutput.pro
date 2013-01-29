; write the wavelet summary to text file
pro writeSummary, smy, smySub, names, filename
	openw, lun, filename, /GET_LUN

	; write the output as text
	; first the header
	printf, lun, 'crystal,mean,median,var,MAD,SD,min,max,energy(%)'
	fmtString = '(7(f18.3, ","),f15.5)'
	lvl = 0
	for cc = 0, n_elements(names) - 1 do begin
		smyStr = names[cc] + ',' + string(smy[*,cc], format=fmtstring)
		printf, lun, smystr
		if ((cc + 1) mod 3 eq 0) then begin
			; write the subtotals per level
			fmtLvl = '(i2)'
			smyStr = 'level' + string(lvl + 1, format=fmtLvl) + ',' + string(smySub[*,lvl], format=fmtString)
			printf, lun, smystr
			lvl +=1
		endif
	endfor

	close, lun
	free_lun, lun
end

; write the wavelet summary to text file
pro writeSummary_Q, smy, smySub, names, filename
  openw, lun, filename, /GET_LUN

  ; write the output as text
  ; first the header
  printf, lun, 'crystal,mean,median,var,MAD,SD,min,max,energy(%),q1_thres,q1_thres'
  fmtString = '(9(f18.3, ","),f15.5)'
  lvl = 0
  for cc = 0, n_elements(names) - 1 do begin
    smyStr = names[cc] + ',' + string(smy[*,cc], format=fmtstring)
    printf, lun, smystr
    if ((cc + 1) mod 3 eq 0) then begin
      ; write the subtotals per level
      fmtLvl = '(i2)'
      smyStr = 'level' + string(lvl + 1, format=fmtLvl) + ',' + string(smySub[*,lvl], format=fmtString)
      printf, lun, smystr
      lvl +=1
    endif
  endfor

  close, lun
  free_lun, lun
end

; write the multiresolution spectrum and power spectrum coefficients
; the power spectrum coefficients are calculated from the multires
; by squaring the individual values.
pro writeOutput, filename, output, also_bin
	dims = size(output, /dim)
	nl = dims[1]
	ns = dims[0]

	openw, lun, filename, /GET_LUN

	; write the output as text
	fmtString = '(' + string(ns - 1) + '(g12.4, " "),g12.4)'
	printf, lun, output, format=fmtstring

	close, lun
	free_lun, lun

	; Write the data also binary
	if (also_bin eq 1) then begin
		dot = strpos(filename, '.', /REVERSE_SEARCH)
		if (dot le 0) then begin
			filebin = filename + '_bin'
		endif else begin
			filebin = strmid(filename, 0, dot) + "_bin"
		endelse
		; Prepare the header for the binary if needed
		ENVI_SETUP_HEAD, fname = filebin, $
			ns = ns, nl = nl, nb = 1, $
			interleave = 0, data_type = 4, $
			file_type = ENVI_FILE_TYPE('ENVI Standard'), $
			descrip= "  Wavelet Coefficients [" + SYSTIME(0) + "] }", $
			/write, /open, xstart = xstart, ystart = ystart

		openw, lun, filebin, /GET_LUN

		; write the output as text
		writeu, lun, output

		close, lun
		free_lun, lun
	endif
end