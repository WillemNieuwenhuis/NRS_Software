;----------
; Write the averaged spectra to a space delimited
; text file. The file will be arranged such that the
; each line contains the responses of the spectra for
; a single wavelength. The spectra can be read by reading
; a column for a spectrum
; parameters:
; 	filename:	the filename to store the spectra
;	avgSpectra:	the 2-dim array containing the spectra
;	pointIDs:	the ID's of all valid points
;	pntCount:	the number of valid points to output
pro writeSpectralCSV, filename, avgSpectra, pointIDs, pntCount
	openw, lun, filename, /get_lun

	; output the spectra to the space delimited file
	; for all spectra in avgSpectra
	cols = size(avgSpectra[*,0], /n_elements)
	bands = size(avgSpectra[0,*], /n_elements)

	; first print the point ID's
	printLine = ''
	for col = 0, pntCount - 1 do begin
		if col gt 0 then printLine = printLine + ','
		printLine = printLine + pointIDs[col]
	endfor
	printf, lun, printLine

	; Now print the spectra
	fmtString = '(f12.3)'
	for band = 0, bands - 1 do begin
		printLine = ''
		for col = 0, pntCount - 1 do begin
			if col gt 0 then printLine = printLine + ','
			printLine = printLine + string(avgSpectra[col,band], format=fmtString)
;			printf, lun, avgSpectra[*,band], format=fmtString
		endfor
		printf, lun, printLine
	endfor

	free_lun, lun
end
