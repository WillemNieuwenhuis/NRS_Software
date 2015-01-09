; Parameters:
;	hymaps		(input) the array of names of the hymap images
;	maps		(output) the list of ID's of all valid maps in the hymaps list
;	minBands	(output) the minimum number of bands encountered in all maps
;	maxBands	(output) the maximum number of bands encountered in all maps
;+
; :description:
;   Open the hymap images to get an ID's for each valid map
;
; :params:
;    hymaps : in
;      the array with names of the hymap images
;    maps : out
;      the list of ID's of all valid maps in the hymaps list
;    minBands : out
;      the minimum number of bands encountered in all maps
;    maxBands : out
;      the maximum number of bands encountered in all maps
;
; :author: nieuwenhuis
; :history:
;   <li>sept 2007 - created
;   <li>may 2013  - renamed
;-
pro nrs_average_spectra_open_image, hymaps, maps, minBands, maxBands
  compile_opt idl2, logical_predicate

	mapCount = size(hymaps, /n_elements)
	min = 9999999
	max = -1
	count = 0
	ids = intarr(mapCount)
	for i = 0, mapCount - 1 do begin
		envi_open_data_file, hymaps[i], r_fid = fid
		if fid eq -1 then continue

		ids[count] = fid
		count = count + 1

		envi_file_query, fid, nb = bandCount
		if min > bandCount then min = bandCount
		if max < bandCount then max = bandCount
	endfor

	minBands = min
	maxBands = max
	maps = intarr(count)
	for i = 0, count - 1 do begin
		maps[i] = ids[i]
	endfor
end

;+
; :description:
;   Calculate the average spectrum for each location
;
; :params:
;    points : in
;      the array of locations
;    mapID : in
;      the ID of the map to be examined
;    avgSpectra : in, out
;      the collection of average spectra one for each point
;    pntCount : in, out
;      counter to keep track of the position in the average spectra
;      array. incremented for each point feature examined.
;    pointIDs : out
;      array with the ID's of all the points that have valid spectra.
;      array follows the same order as the avgSpectra array
;    tranq : in
;      the report window ID
;    mapCount : in
;      number of maps (tiles) being processed
;    tranq_map : in
;      map number currently being processed
;
; :keywords:
;    kernel : in, optional, default = 7
;      size of the window around the location in pixels (default 7 x 7). Is limited to 3, 5 or 7
;
; :author: nieuwenhuis
; :history:
;   <li>sept 2007 - created
;   <li>may 2013  - renamed; added kernel keyword
;-
pro nrs_average_spectra_single, points, mapID $
                              , avgSpectra, pntCount, pointIDs, tranq, mapCount, tranq_map $
                              , kernel = kernel
  compile_opt idl2, logical_predicate

  if n_elements(kernel) eq 0 then kernel = 7 $
  else kernel = min([7, max([3, kernel])])  ; kernel = 3,5,7
  kern2 = fix(kernel / 2)
  
	envi_file_query, mapID, nb = nb, nl = lines, ns = columns, data_ignore_value = undef
	
	trq_last = mapCount * pntCount
	trq_pos = tranq_map * pntCount

	pointCount = size(points, /n_elements)
	for i = 0, pointCount - 1 do begin
		envi_report_stat, tranq, trq_pos, trq_last, cancel = cancel
		trq_pos += 1

		; retrieve the spectral values of the feature location
		; get the coordinate (assumption it is a point)
		coordX = points[i].X
		coordY = points[i].Y
		; translate to pixel location
		envi_convert_file_coordinates, mapID, pixelX, pixelY, coordX, coordY
		; only need to handle the point if it is i the current map
		if nrs_check_bounds(pixelX - kern2, pixelY - kern2, lines, columns) eq 0 then continue
		if nrs_check_bounds(pixelX + kern2, pixelY + kern2, lines, columns) eq 0 then continue

		; calculate the average spectrum for the point location
		; by looking in an area of kernel x kernel pixels
		specTotal = fltarr(kernel, nb)
		for line = pixelY - kern2, pixelY + kern2 do begin
			spectrum = envi_get_slice(fid = mapID, line = line, xs = pixelX - kern2, xe = pixelX + kern2, /bil)
  		specTotal += total(spectrum, 1)
		endfor

		; very simple check for valid area; assuming zero for out of area
		if total(specTotal) le 10 then continue

		avgSpectra[pntCount, *] = specTotal / (kernel ^ 2)
		pointIDs[pntCount] = points[i].ID
		pntCount = pntCount + 1
	endfor
end

; TODO: improve the majority ranking
;+
; :description:
;   Calculate the average spectrum for each point after a majority filter
;
; :params:
;   points : in
;     the array of points to iterate along
;   mapID : in
;     the ID of the map to be examined for spectra
;   avgSpectra : in/out
;     the collection of average spectra one for each point
;   pntCount : in/out
;     counter to keep track of the position in the average spectra
;     array; incremented for each point feature examined.
;   pointIDs : out
;     array with the ID's of all the points that have valid spectra.
;     array follows the same order as the avgSpectra array
;   tranq : in
;     the report window ID
;   mapCount : in
;     number of maps (tiles) being processed
;   tranq_map : in
;     map number currently being processed
;   threshold : in
;     determine how much min and max values must differ
;     before deciding that there are outliers
;
; :keywords:
;    kernel : in, optional, default = 7
;      size of the window around the location in pixels (default 7 x 7). Is limited to 3, 5 or 7
;
; :author: nieuwenhuis
; :history:
;   <li>sept 2007 - created
;   <li>may 2013  - renamed; added kernel keyword
;-
pro nrs_average_spectra_rank_single, points, mapID $
                 , avgSpectra, pntCount, pointIDs $
                 , tranq, mapCount, tranq_map, threshold $
                 , kernel = kernel
  compile_opt idl2, logical_predicate

  if n_elements(kernel) eq 0 then kernel = 7 $
  else kernel = min([7, max([3, kernel])])  ; kernel = 3,5,7
  kern2 = fix(kernel / 2)
  
	envi_file_query, mapID, nb = nb, nl = lines, ns = columns

	trq_last = mapCount * pntCount
	trq_pos = tranq_map * pntCount

	pointCount = size(points, /n_elements)
	rankColl = dblarr(kernel, kernel, /nozero)
	cube = fltarr(kernel, kernel, nb)
	for i = 0, pointCount - 1 do begin
		envi_report_stat, tranq, trq_pos, trq_last, cancel = cancel
		trq_pos += 1

		coordX = points[i].X
		coordY = points[i].Y
		envi_convert_file_coordinates, mapID, pixelX, pixelY, coordX, coordY

		if nrs_check_bounds(pixelX - kern2, pixelY - kern2, lines, columns) eq 0 then continue
		if nrs_check_bounds(pixelX + kern2, pixelY + kern2, lines, columns) eq 0 then continue

    rankColl[*, *] = 0.0D
		lnCount = 0
    cube = reform(cube, kernel, kernel, nb, /overwrite)
		for line = pixelY - kern2, pixelY + kern2 do begin
			spectrum = envi_get_slice(fid = mapID, line = line, xs = pixelX - kern2, xe = pixelX + kern2, /bil)
			cube[*, lnCount, *] = spectrum
			rankColl[*, lnCount] += total(spectrum, 2)
			lnCount = lnCount + 1
		endfor
		cube = reform(cube, kernel * kernel, nb, /overwrite)

		; very simple check if the point does not lie in black area
		if (total(rankColl) le 100) then continue

		minV = min(rankColl, max = maxV)
		range = maxV - minV
		perc =  minV * threshold
		; only calculate predominant spectra if the min and max
		; values differ more than a threshold value
		if (minV + perc) lt maxV then begin
		  his = histogram(rankColl, rev = ri, nbins = kernel, min = minV)
		  maxbin = max(his, im)
		  meanval = mean(ri[ri[im] : ri[im+1]-1])
		  
		  ix = where(abs(rankColl - meanval) lt perc / 2, cnt)
		  specTotal = total(cube[ix, *], 1) / cnt
		endif else begin
      specTotal = total(cube, 1) / (kernel ^ 2)
		endelse

		avgSpectra[pntCount,*] = specTotal
		pointIDs[pntCount] = points[i].ID
		pntCount = pntCount + 1
	endfor
end

;----------
; Cleanup all open resources, and remove all open objects
pro closeAllResources, maps
  compile_opt idl2, logical_predicate

	; Close the hyperspectral images
	for i = 0, size(maps, /n_elements) - 1 do begin
		envi_file_mng, id = maps[i], /remove
	endfor

END

;+
; :description:
;   Calculate the average spectra in an array of 7 by 7 pixels
;   around point locations.
;   The calculation is done for each separate point location.
;   The point locations are stored in a series of shape files
;   the spectra are store in hyperspectral images
;
;   The point locations are ordered to minimize the access to
;   the different hyperspectral images.
;
; :params:
;    shapes : in
;      a list of names of the shape files
;    hymaps : in
;      a list of names of hyperspectral images
;    outputFilename : in
;      the name of the file to store the results
;    threshold : in
;      the threshold value in case of majority calculation
;
; :keywords:
;    kernel : in, optional, default = 7
;      size of the kernel (3, 5 or 7)
;
; :author: nieuwenhuis
; :history:
;   <li>sept 2007 - created
;   <li>may 2013  - renamed; added kernel keyword
;-
pro nrs_average_spectrum, shapes, hymaps, outputFilename, threshold, kernel = kernel
  compile_opt idl2, logical_predicate

	points = nrs_read_shape_points(shapes)
	points = points[sort(points.S)]

	; maps contains the file ID's of all hyperspectral maps
	nrs_average_spectra_open_image, hymaps, maps, minBands, maxBands
	if minBands ne MaxBands then begin
		ans = dialog_message("The number of hyperspectral bands are not the same in all maps; make sure you only select maps that have equal number of wavelengths", /error, title="Fatal error")
		closeAllResources, maps
		exit
	endif

	; open the hyperspectral images one by one
	; get the boundaries of each and then ..
	; iterate through the points
	mapCount = size(maps, /n_elements)
	bands = minBands
	pointCount = size(points, /n_elements)
	allAverageSpectra = dblarr(pointCount * mapCount, bands)
	pointIDs = strarr(pointCount * mapCount)
	pntCount = 0

	; initialise tranquilizer
	envi_report_init,["Performing spectrum extraction",$
                      "This can take a few minutes"],$
						base = tranq, title = "progress"
	tranqstep = 1
	envi_report_inc, tranq, tranqstep

	if abs(threshold) le 0.0000001 then begin
		for i = 0, mapCount - 1 do begin
			fid = maps[i]
			nrs_average_spectra_single, points, fid, allAverageSpectra, pntCount, $
					pointIDs, tranq, mapCount, i
		endfor
	endif else begin
		for i = 0, mapCount - 1 do begin
			fid = maps[i]
			nrs_average_spectra_rank_single, points, fid, allAverageSpectra, pntCount,$
					pointIDs, tranq, mapCount, i, threshold
		endfor
	endelse

	actualPoints = pointIDs[0:(pntCount - 1)]
	actualSpectra = allAverageSpectra[0:(pntCount - 1),*]
	pntSort = sort(actualPoints)
	ids = actualPoints(pntSort)
	avgSpect = actualSpectra[pntSort,*]

	nrs_spectral_write_table, outputFilename, avgSpect, ids, pntCount

	envi_report_init, base=tranq, /finish

	closeAllResources, maps
end
