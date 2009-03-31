; Author: Willem Nieuwenhuis, march 2009
; write the statistics to text files
pro writeStatistics, band_stat, col_stats, type, filename
	; write the band statistcs first
	; Columns:
	;	destripe	= band being destriped
	;	type		= algorithm used
	;	src_band	= source band used for destriping (-1 for entire image)
	;	mean		= mean of the band/image
	;	stddev		= standard deviation of the band/image
	;
	outname = filename + '_bandstats_' + string(type, format='(i0)') + '.csv'
	openw, lun, outname, /GET_LUN
	; first the header
	printf, lun, 'destripe,type,src_band,mean,stddev'

	; then handle the band statistics
	fmtString = '(3(i0,","),2(f12.3, ","))'
	rows = n_elements(band_stat[0, *])
	for row = 0, rows - 1 do begin
		smyStr = string(band_stat[*, row], format = fmtstring)
		printf, lun, smystr
	endfor

	close, lun
	free_lun, lun

	; Now write the column statistics
	; Columns:
	;	destripe	= band being destriped
	;	column		= column number in the band being destriped
	;	mean		= mean of the column
	;	stddev		= standard deviation of the column
	;
	;	The columns [column, mean, stddev] are repeated for all columns in the band
	;
	outname = filename + '_colstats_' + string(type, format='(i0)') + '.csv'
	openw, lun, outname, /GET_LUN
	; first the header
	header_str = 'band'
	col_count = (n_elements(col_stats[*, 0]) - 1) / 3
	for c = 0, col_count - 1 do $
		header_str += ',column,mean,stddev'
	printf, lun, header_str

	fmtstring = '(i0,","' + string(col_count, format = '(i0)') + '(i0,",",f12.3,",",f12.3, ","))'
	rows = n_elements(col_stats[0, *])
	for row = 0, rows - 1 do begin
		smyStr = string(col_stats[*, row], format = fmtstring)
		printf, lun, smystr
	endfor

	close, lun
	free_lun, lun
end
