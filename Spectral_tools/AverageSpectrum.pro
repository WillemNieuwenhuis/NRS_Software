
; sort the point locations in the shape files such that
; several sequences of point locations are created, each
; sequence overlapping with one hymap image
; parameters:
;	shapes:	the list of names of the shape files
function sortPoints, shapes
	; first read all the points from the shape files
	; into an array of point locations
	coordinate = {Coordinate, $
				X	: double(0), $
				Y	: double(0), $
				S	: double(0), $  ; value used for sorting
				ID	: string('') $
				}
	coords = replicate(coordinate, 1000)
	; start by reading all shapes to get the total point count
	shapeCount = size(shapes, /n_elements)
	entityCount = 0
	for shp = 0, shapeCount - 1 do begin
		; open the input shapefile
		shape = OBJ_NEW('IDLffShape', shapes[shp])

   		; Get the number of entities and the entity type.
		shape->IDLffShape::GetProperty, N_ENTITIES = num_ent, $
   			ENTITY_TYPE = ent_type, N_ATTRIBUTES = num_attr

		if ent_type NE 1 then begin  ; 1 == Point
			OBJ_DESTROY, shape
			continue
		endif

		for sh = 0, num_ent - 1 do begin
			; get the feature: only the location is needed
			feature = shape->IDLffShape::GetEntity(sh, /attributes)
			coords[entityCount].X = feature.bounds[0]
			coords[entityCount].Y = feature.bounds[1]
			coords[entityCount].S = feature.bounds[1] * 10000000 + feature.bounds[0]

			; Get the (string) ID of the feature
			if num_attr gt 0 then begin
				shape->IDLffShape::GetProperty, ATTRIBUTE_INFO = attr_info
				for at = 0, num_attr - 1 do begin
					if (strupcase(attr_info[at].name) eq 'ID') then begin
						attr = feature.attributes
						sid = (*attr).(at)
						coords[entityCount].ID = sid
					endif
				endfor
			endif else begin
				continue  ; skip this feature if it has no ID
			endelse

			entityCount = entityCount + 1
		endfor

		OBJ_DESTROY, shape
	endfor

	; sort the point locations
	sorted = replicate(coordinate, entityCount)
	for i = 0, entityCount - 1 do begin
		sorted[i] = coords[i]
	endfor
	sorted = sorted(sort(sorted.S))

	return, sorted
end

; Open the hymap images to get an ID's for each valid map
; Parameters:
;	hymaps		(input) the array of names of the hymap images
;	maps		(output) the list of ID's of all valid maps in the hymaps list
;	minBands	(output) the minimum number of bands encountered in all maps
;	maxBands	(output) the maximum number of bands encountered in all maps
pro openMaps, hymaps, maps, minBands, maxBands
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

;----------
; Check the whether a pixel is within the bounds of the image
; Return:
;	- "1" if the pixel is within the image
;	- "-1", if the pixel lies outside of the image
function checkBounds, x, y, lines, columns
	if (x ge 0) and (x lt columns) and $
	   (y ge 0) and (y lt lines) then return, 1
	return, -1
end

; Calculate the average spectrum for each point
; Parameters:
;	points		(input) the array of points to iterate along
;	mapID		(input) the ID of the map to be examined for spectra
;	avgSpectra	(in/out) the collection of average spectra one for each point
;	pntCount	(in/out) counter to keep track of the position in the average spectra
;				array; incremented for each point feature examined.
;	pointIDs	(output) array with the ID's of all the points that have valid spectra.
;				array follows the same order as the avgSpectra array
;	tranq		(input) the report window ID
;	mapCount	(input) number of maps (tiles) being processed
;	tranq_map	(input) map number currently being processed
pro averageSpectraInSingleMap, points, mapID, avgSpectra, pntCount, pointIDs, tranq, mapCount, tranq_map
	envi_file_query, mapID, nb = bandCount, nl = lines, ns = columns

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
		if checkBounds(pixelX - 3, pixelY - 3, lines, columns) eq - 1 then continue
		if checkBounds(pixelX + 3, pixelY + 3, lines, columns) eq - 1 then continue

		; calculate the average spectrum for the point location
		; by looking in an area of 7 x 7 pixels
		init = 0
		for line = pixelY - 3, pixelY + 3 do begin
			; get the spectral values for one line
			spectrum = envi_get_slice(fid = mapID, line = line, xs = pixelX - 3, xe = pixelX + 3, /bil)
			; sum to a spectrum total
			if (init eq 0) then begin
				specTotal = total(spectrum, 1)
				init = 1
			endif else begin
				specTotal = specTotal + total(spectrum, 1)
			endelse
		endfor

		; check if the point does not lie in black area
		; if so skip to next point
		sum = total(specTotal)
		if (sum le 10) then continue

		; non-trivial spectrum
		avgSpectra[pntCount,*] = specTotal / 49
		pointIDs[pntCount] = points[i].ID
		pntCount = pntCount + 1
	endfor
end

; TODO: improve the majority ranking
; Calculate the average spectrum for each point after a majority filter
; Parameters:
;	points		(input) the array of points to iterate along
;	mapID		(input) the ID of the map to be examined for spectra
;	avgSpectra	(in/out) the collection of average spectra one for each point
;	pntCount	(in/out) counter to keep track of the position in the average spectra
;					array; incremented for each point feature examined.
;	pointIDs	(output) array with the ID's of all the points that have valid spectra.
;					array follows the same order as the avgSpectra array
;	tranq		(input) the report window ID
;	mapCount	(input) number of maps (tiles) being processed
;	tranq_map	(input) map number currently being processed
; 	threshold	(input) determine how much min and max values must differ
;					before deciding that there are outliers
pro majorityAverageSpectraInSingleMap, points, mapID, avgSpectra, pntCount, pointIDs, tranq, mapCount, tranq_map, threshold
	envi_file_query, mapID, nb = bandCount, nl = lines, ns = columns

	trq_last = mapCount * pntCount
	trq_pos = tranq_map * pntCount

	pointCount = size(points, /n_elements)
	rankColl = dblarr(7, 7, /NOZERO)
	participants = intarr(7, 7, /NOZERO)
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
		if checkBounds(pixelX - 3, pixelY - 3, lines, columns) eq - 1 then continue
		if checkBounds(pixelX + 3, pixelY + 3, lines, columns) eq - 1 then continue

		; clear the rankorder array
		rankColl[*, *] = 0.0D
		; calculate the spectrum sum (other algorithms might be more appropriate
		; such as: spectral angle, real euclidian distance, etc)
		; for all pixels in an area of 7 x 7 pixels
		lnCount = 0
		for line = pixelY - 3, pixelY + 3 do begin
			; get the spectral values for one line
			spectrum = envi_get_slice(fid = mapID, line = line, xs = pixelX - 3, xe = pixelX + 3, /bil)
			rankColl[*, lnCount] = rankColl[*, lnCount] + total(spectrum, 2)
			lnCount = lnCount + 1
		endfor

		; check if the point does not lie in black area
		; if so skip to next point
		sum = total(rankColl)
		if (sum le 100) then continue

		; we have a 7 x 7 array: for each pixel the value is the sum of
		; the reflectance values for the spectrum vector at that location
		; Now order the values to find the majority
		sorted = reform(sort(rankColl), 7, 7)

		minV = rankColl[sorted[0, 0]]
		maxV = rankColl[sorted[6, 6]]
		selCount = 0
		specTotal = dblarr(1, bandCount)

		; increase the range just a little bit; makes sure that
		; the calculated integer index will always be < 7
		range = maxV - minV + 0.001
		perc =  minV * threshold
		; only calculate predominant spectra if the min and max
		; values differ more than a threshold value
		if (minV + perc) lt maxV then begin
			; we need majority ranking calculation
			hist = intarr(7)  ; simple histogram bins (7 equidistant)
			histVal = dblarr(7)
			for h = 0, 48 do begin
				val = rankColl[h] ; no need for exact index, linear will do
				index = fix(7 * (val - minV) / range) ; guarantueed < 7
				hist[index] = hist[index] + 1		; count # in this bin
				histVal[index] = histVal[index] + val	; sum values in this bin
			endfor
			maxbin = max(hist, indexMax) ; maxbin is counter in the bin
			avgVal = histVal[indexMax] / maxbin	; average of the summed spectra

			; now use avgVal and the threshold to select those spectra to be
			; considered for the actual spectrum average
			thresHalf = perc / 2
			; clear the array flagging the selected spectra
			participants[*, *] = 0
			for h = 0, 48 do begin
				val = rankColl[h] ; no need for exact index, linear will do
				if abs(avgVal - val) lt thresHalf then participants[h] = 1
			endfor
			; calculate the average spectrum for the point location
			; by looking at the selected spectra in the area of 7 x 7 pixels
			selCount = 0
			lnCount = 0
			for line = pixelY - 3, pixelY + 3 do begin
				colCount = 0
				for col = pixelX - 3, pixelX + 3 do begin
					; if not in the selection skip to the next
					if participants[lnCount, colCount] ne 0 then begin
						; get the spectral values for the selected pixel
						spectrum = envi_get_slice(fid = mapID, line = line, xs = col, xe = col, /bil)
						selCount = selCount + 1
						; sum to a spectrum total
						specTotal = specTotal + spectrum
					end
					colCount = colCount + 1
				endfor
				lnCount = lnCount + 1
			endfor
		endif else begin
			; calculate the average spectrum for the point location
			; by looking in an area of 7 x 7 pixels
			for line = pixelY - 3, pixelY + 3 do begin
				; get the spectral values for one line
				spectrum = envi_get_slice(fid = mapID, line = line, xs = pixelX - 3, xe = pixelX + 3, /bil)
				; sum to a spectrum total
				specTotal = specTotal + total(spectrum, 1)
			endfor
			selCount = 49 ; all spectra have been used
		endelse

		; non-trivial spectrum
		avgSpectra[pntCount,*] = specTotal / selCount
		pointIDs[pntCount] = points[i].ID
		pntCount = pntCount + 1
	endfor
end

;----------
; Cleanup all open resources, and remove all open objects
pro closeAllResources, maps
	; Close the hyperspectral images
	for i = 0, size(maps, /n_elements) - 1 do begin
		envi_file_mng, id = maps[i], /remove
	endfor

END

; Calculate the average spectra in an array of 7 by 7 pixels
; around point locations.
; The calculation is done for each separate point location.
; The point locations are stored in a series of shape files
; the spectra are store in hyperspectral images
; parameters:
;	- shapes:	a list of names of the shape files
;	- hymaps:	a list of names of hyperspectral images
;	- outputFilename:	the name of the file to store the results
;	- threshold the threshold value in case of majority calculation
; The point locations are ordered to minimize the access to
; the different hyperspectral images.
pro AverageSpectrum, shapes, hymaps, outputFilename, threshold
	; sort the point locations; use the boundaries of the hymap
	; images each hymap image can be accessed in sequence.
	points = sortPoints(shapes)

	; maps contains the file ID's of all hyperspectral maps
	openMaps, hymaps, maps, minBands, maxBands
	if minBands ne MaxBands then begin
		dialog_message, "The number of hyperspectral bands are not the same in all maps; \nmake sure you only select maps that have equal number of wavelengths", /error, title="Fatal error"
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
			averageSpectraInSingleMap, points, fid, allAverageSpectra, pntCount, $
					pointIDs, tranq, mapCount, i
		endfor
	endif else begin
		for i = 0, mapCount - 1 do begin
			fid = maps[i]
			majorityAverageSpectraInSingleMap, points, fid, allAverageSpectra, pntCount,$
					pointIDs, tranq, mapCount, i, threshold
		endfor
	endelse

	actualPoints = pointIDs[0:(pntCount - 1)]
	actualSpectra = allAverageSpectra[0:(pntCount - 1),*]
	pntSort = sort(actualPoints)
	ids = actualPoints(pntSort)
	avgSpect = actualSpectra(pntSort,*)

	writeSpectralCSV, outputFilename, avgSpect, ids, pntCount

	envi_report_init, base=tranq, /finish

	closeAllResources, maps
end
