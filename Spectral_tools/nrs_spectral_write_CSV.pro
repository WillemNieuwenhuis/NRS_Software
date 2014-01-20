;+
; :description:
;   Write the averaged spectra to a space delimited
;   text file. The file will be arranged such that the
;   each line contains the responses of the spectra for
;   a single wavelength. The spectra can be read by reading
;   a column for a spectrum
;
; :params:
;    filename: in, required
;      the filename to store the spectra
;    avgSpectra : in, required
;      the 2-dim array containing the spectra (points (X) x spectral profile (Y))
;    pointIDs : in, required
;      the ID's of all valid points
;    pntCount : in, required
;      the number of valid points to output
;
; :author: nieuwenhuis
; :history:
;   <li>sept 2007 - created
;   <li>may 2013  - rewritten
;-
pro nrs_spectral_write_table, filename, avgSpectra, pointIDs, pntCount
  compile_opt idl2, logical_predicate

	openw, lun, filename, /get_lun

	printf, lun, strjoin(string(pointIDs, format = '(i0)'), ',')

	for band = 0, n_elements(avgSpectra[0,*]) - 1 do begin
		printf, lun, strjoin(string(avgSpectra[*, band], format = '(f12.3)'), ',')
	endfor

	free_lun, lun
end
