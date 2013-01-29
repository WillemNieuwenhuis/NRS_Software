pro nrs_timeseries_from_table, table_name
  compile_opt idl2, logical_predicate
  
  openr, unit, table_name, /get_lun
  
  line = ''
  readf, unit, line
  if strpos(line, ',') gt 0 || strpos(line, ';') gt 0 then $
    parts = strsplit(strlowcase(line), ',;', /extract) $
  else $
    parts = strsplit(strlowcase(line), /extract)
  var_name = strtrim(parts[0], 2)

  readf, unit, line
  parts = strsplit(strlowcase(line), '[ ' + STRING(9B) + ',;]+', /extract, /regex)
  if ~((parts[0] eq 'year') && (parts[4] eq 'lat.')) then begin
    ans = dialog_message('Table format not recognized')
    close, unit
    return
  endif
  lons = float(parts[5 : n_elements(parts) - 1])
  ns = n_elements(lons)
  bnames = []
  img = []
  min_lat = 1000.0
  max_lat = -1.0
  lat_step = 1.0
  lat_cnt = 0
  nl = 0
  repeat begin
    fmap = []
    fend = 0
    while ~eof(unit) && ~fend do begin
      readf, unit, line
      parts = strsplit(strlowcase(line), '[ ' + STRING(9B) + ',;]+', /extract, /regex)
      fend = (parts[0] eq 'year') && (parts[4] eq 'lat.')
      if ~fend then begin
        cur_year = strtrim(parts[0], 2)
        fmap = [fmap, line]
      endif
    endwhile
    
    nl = max([n_elements(fmap), nl])
    
    lats = []
    band = fltarr(ns, nl)
    parts = strsplit(fmap, '[ ' + STRING(9B) + ',;]+', /extract, /regex)
    for l = 0, n_elements(fmap) - 1 do begin
      p = parts[l]
      band[*, l] = float(p[5 : 5 + ns - 1])
      lats = [lats, float(p[4])]
    endfor
    bnames = [bnames,  p[1]]
    min_lat = min([min_lat, lats])
    max_lat = max([max_lat, lats])
    n_elem = n_elements(lats)
    lat_cnt = max([lat_cnt, n_elem])
    if n_elem lt lat_cnt then begin
      si = lats[0] - min_lat
      ei = min([lats[-1] - min_lat, nl])
      bt = fltarr(ns, nl)
      bt[*, si : ei] = band[*, 0 : ei - si]
      band = bt
    endif
    if lats[0] lt lats[-1] then begin
      for l = 0, nl / 2 - 1 do begin
        tmp = band[*, l]
        band[*, l] = band[*, nl - l - 1]
        band[*, nl - l - 1] = tmp
      endfor
    endif
    img = [img, band]
  endrep until eof(unit)

  close, unit

  outname = getOutname(table_name, basename = var_name, postfix = '_' + cur_year, ext = '.')
  mi = envi_map_info_create(/geographic, mc = [0, 0, min(lons), max_lat], ps = [lons[1] - lons[0], lats[1] - lats[0]])
  nb = n_elements(bnames)
  envi_write_envi_file, out_name = outname, img, bnames = bnames $
                      , map_info = mi $
                      , ns = ns, nl = nl, nb = nb, interleave = 1 ; BIL
  
end
