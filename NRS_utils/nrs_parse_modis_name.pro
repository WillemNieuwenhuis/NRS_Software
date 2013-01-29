;
function nrs_parse_modis_name, modis_name
	modis_fields = { modis_struct, $
		is_terra:	0, $
		is_tiled: 0, $
		produc:		'', $
		year:		0L, $
		jul_day:	0L, $
		hour:     0L, $
		minute:   0L, $
		version:	0L, $
		tile_h:		0L, $
		tile_v:		0L, $
		prod_dt:	'' $
	}
  ; first remove path
  modis_name = file_basename(modis_name)

	pr = strmid(modis_name, 0, 3)
	if (pr ne 'MOD') and (pr ne 'MYD') then return, -1	; no MODIS product

	modis_fields.is_terra = (pr eq 'MOD')
	pos = strpos(modis_name, '.')
	modis_fields.produc = strmid(modis_name, 0, pos)
	modis_fields.year = fix(strmid(modis_name, pos + 2, 4))
	modis_fields.jul_day = fix(strmid(modis_name, pos + 6, 3))
	pos2 = strpos(modis_name, '.', pos + 1)
	pos3 = strpos(modis_name, '.', pos2 + 1)
	is_tiled = (pos3 - pos2) ge 6
	if is_tiled then begin
  	modis_fields.tile_h = fix(strmid(modis_name, pos2 + 2, 2))
  	modis_fields.tile_v = fix(strmid(modis_name, pos2 + 5, 2))
	endif else begin
	  modis_fields.hour = fix(strmid(modis_name, pos2 + 1, 2))
	  modis_fields.minute = fix(strmid(modis_name, pos2 + 3, 2))
	endelse
	pos4 = strpos(modis_name, '.', pos3 + 1)
	modis_fields.version = fix(strmid(modis_name, pos3 + 1, pos4 - pos3 - 1))
	pos5 = strpos(modis_name, '.', pos4 + 1)
	modis_fields.prod_dt = strmid(modis_name, pos4 + 1, pos5 - pos4 - 1)

	return, modis_fields
end

function nrs_get_modis_name, mf
	name = mf.produc + '.' + string(mf.year, format = '(i4)') + string(mf.jul_day, format = '(i03)')
	return, name
end
