; get the field value for a parameter
; Parameters:
;	index:	the index number of the parameter
;			if the index is -1 then return all values for the field index
;	field:	the field index of the parameter
;			- 0 = name
;			- 1 = designation
;			- 2 = unit
;			- 3 = minimum value
;			- 4 = maximum value
;			- 5 = default value
; If the field index == -1 then all the properties of a single parameter are returned
function getParameterField, index, field
	pfields = { $
		name:		'', $
		desg:		'', $
		unit:		'', $
		min_val:	0.0, $
		max_val:	0.0, $
		def_val:	0.0,  $
		format:		'' $
	}

	field_names	 = ['leaf structure parameter', 'chlorophyll a+b content', $
					'equivalent water thickness', 'dry matter content', 'Hot spot', $
					'Scale factor for soil reflectance', 'Single tree leaf area index', $
					'Leaf area index of understorey', 'Stem density', 'Tree height', $
					'Crown diameter', 'Average leaf angle of tree canopy', $
					'Sun zenith angle', 'Observation zenith angle', 'Azimuth angle', $
					'Fraction of diffuse radiation']
	field_desig  = ['N','Cab','Cw','Cm','hot','scale','LAIs','LAIu', $
					'sd','h','cd','ala','teta_s','teta_o','phi','skyl']
	field_units  = ['-','µg/cm²','g/cm²','g/cm²','-','-','m²/m²','m²/m²', $
					'1/ha','m','m','deg','deg','deg','deg','Fraction']
	field_mins	 = [1,0,0.0,0.002,0.01,0,0,0,0,0,0,0,0,0,0,0.0]
	field_maxs   = [3,100,0.05,0.02,0.05,1,10,3,4000,50,10,89,89,89,180,1.0]
	field_defs	 = [2,60,0.025,0.025,0.02,1,7,0.1,650,20,4.5,55,30,0,0,0.1]
	field_format = ['(f0.1)','(f0.1)','(f0.3)','(f0.3)','(f0.3)','(f0.1)','(f0.1)','(f0.2)', $
					'(i0)'  ,'(i0)'  ,'(f0.1)','(f0.1)','(f0.1)','(f0.1)','(f0.1)','(f0.2)']

	if index eq -1 then begin
		switch field of
			0 : return, field_names
			1 : return, field_desig
			2 : return, field_units
			3 : return, field_mins
			4 : return, field_maxs
			5 : return, field_defs
			6 : return, field_format
		endswitch
	end

	if index ge n_elements(field_names) then begin
		if field le 2 then return, ''
		if field gt 2 then return, 0.0
	endif

	if field eq -1 then begin
		pfields.name = field_names[index]
		pfields.desg = field_desig[index]
		pfields.unit = field_units[index]
		pfields.min_val = field_mins[index]
		pfields.max_val = field_maxs[index]
		pfields.def_val = field_defs[index]
		pfields.format = field_format[index]
		return, pfields
	endif

	switch field of
		0 : return, field_names[index]
		1 : return, field_desig[index]
		2 : return, field_units[index]
		3 : return, field_mins[index]
		4 : return, field_maxs[index]
		5 : return, field_defs[index]
		6 : return, field_format[index]
	endswitch
end

function getIndexOfParam, name
	field_names	 = ['leaf structure parameter', 'chlorophyll a+b content', $
					'equivalent water thickness', 'dry matter content', 'Hot spot', $
					'Scale factor for soil reflectance', 'Single tree leaf area index', $
					'Leaf area index of understorey', 'Stem density', 'Tree height', $
					'Crown diameter', 'Average leaf angle of tree canopy', $
					'Sun zenith angle', 'Observation zenith angle', 'Azimuth angle', $
					'Fraction of diffuse radiation']
	for i = 0, n_elements(field_names) - 1 do begin
		if field_names[i] eq name then return, i
	endfor

	return, -1
end

function getParameterCount
	return, 16
end

; read parameters from comma-delimited file:
; It has one header line, and six fields
function readParamsFromFile, filename
	file_template = { $
		version:		float(1.0),	$
   		datastart:		long(1),	$	; one header line
   		delimiter:		byte(44),	$	; 44 = comma ?
   		missingvalue:	float(!VALUES.F_NAN),	$
   		commentsymbol:	string(''),	$
   		fieldcount:		long(6),	$	; 6 fields
   		fieldtypes:		long([7,7,7,4,4,4]), $	; 7 = string, 4 = float
   		fieldnames:		string(['Name','Designation','Unit','Min','Max','Default']), $
   		fieldlocations:	long([0,27,31,35,37,39]), $
   		fieldgroups:	long([0,1,2,3,4,5]) $
   	}

	fields = read_ascii(filename, template = file_template)

	return, fields
end