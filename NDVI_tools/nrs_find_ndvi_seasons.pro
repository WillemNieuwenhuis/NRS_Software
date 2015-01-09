function nrs_get_jul, i, aggr, per_year, syear, sjul, roundup = roundup
  ioff = (sjul - 1) / aggr
  if ((sjul - 1) mod aggr ne 0) and (n_elements(roundup) gt 0) then ioff++
  i_y = (i + ioff) mod per_year ; period in current year (1..per_year)
  ny = (i + ioff) / per_year  ; increase of year since start

  jul = i_y * aggr + 1
  year = syear + ny
  return, [year, jul]
end

;pro testgetjul
;  print, 'Test1'
;  syear = 2005
;  sjul = 1
;  for i=0,10 do begin
;    dmy = nrs_get_jul(i, 16, 23, syear, sjul)
;    print,dmy
;  endfor
;  print, 'Test2'
;  syear = 2005
;  sjul = 280
;  for i=0,10 do begin
;    dmy = nrs_get_jul(i, 16, 23, syear, sjul)
;    print,dmy
;  endfor
;end
;
;----------
; Main routine
pro nrs_find_ndvi_seasons, imagefile, shapefile, outputfile, start_date = start_date, aggr_period = aggr_period
  envi_open_file, imagefile, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then return
  
  envi_file_query, fid, nb = nb, nl = nl, ns = ns, data_ignore_value = div
  
  has_undef = div ne 1.0e-34
  
  openShapeFile, shapefile, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info
  if ent_type mod 10 ne 1 then begin
    res = dialog_message('Only point features are allowed', title = 'Error', /error)
    return
  endif

  rstr = ["Extracting (potential) seasons in NDVI stack"]
  envi_report_init, rstr, base = tranq, title = "Processing"

  case aggr_period of 
    16 : per_year = 23
    10 : per_year = 36
    else: per_year = 360 / aggr_period
  endcase
  nr_periods = nb / per_year + 1
  
  syear = start_date / 10000
  md = start_date mod 10000
  sjul = nrs_julian_from_dmy(md)
  
;  season_smy = { $
;    date_str:  '', $
;    dmy:       0L, $
;    year:      0,  $
;    julian:    0   $
;  }
;  
;  season_rec = replicate(season_smy, nr_periods)
  
  seasons = make_array(nr_periods, num_ent, /string)
  for i = 0, num_ent - 1 do begin
    envi_report_stat, tranq, i, num_ent
    ; get the feature and its attributes
    feature = myshape->IDLffShape::GetEntity(i, /attributes)
    
    coordX = feature.bounds[0]
    coordY = feature.bounds[1]
    ; translate to pixel location; also check if the pixel position
    ; lies inside the map -> if not skip to the next feature
    envi_convert_file_coordinates, fid, pixelX, pixelY, coordX, coordY
    if nrs_check_bounds(pixelX, pixelY, nl, ns) eq 0 then continue
    
    ; get the spectral values
    spectrum = envi_get_slice(fid = fid, line = pixelY, xs = pixelX, xe = pixelX, /bil)
    spectrum = reform(spectrum, nb)
    if has_undef eq 1 then begin
      uu = where(spectrum eq div, countUndef)
      if countUndef gt 1 then continue
    endif

    spec1 = [spectrum, spectrum[nb - 1]]
    spec2 = [spectrum[0], spectrum]
    
    slope = (spec2 - spec1) ge 0
    slb1 = slope[1 : nb]
    slb2 = slope[0 : nb - 1]
    change = slb2 - slb1
    cix = where(change eq 1)  ; only minimum needed
    ccmin = min(spectrum, max = ccmax)
    cchalf = (ccmin + ccmax) / 1.5
    minsel = cix[where(spectrum[cix] lt cchalf)]

    for p = 0, nr_periods - 1 do begin
      y_jul = nrs_get_jul(minsel[p], aggr_period, per_year, syear, sjul)
      year_str = nrs_julian_as_string(y_jul[1])
      seasons[p, i] = year_str
    endfor
    
    ; cleanup for the feature structure
    myshape->IDLffShape::DestroyEntity, feature
  end
  
  myshape->IDLffShape::Close
  
  hix = indgen(nr_periods) + 1
  head_ar = string(hix, format = '("p",i0)')
  fmt = '(a,' + string(nr_periods - 1, format = '(i0)') + '(",",a))' 
  header = string(head_ar, format = fmt)
  openw, lun, outputfile, /GET_LUN
  printf, lun, header
  
  for i = 0, num_ent - 1 do begin
    season = string(seasons[*, i], format = fmt)
    printf, lun, season
  endfor
  
  close, lun
  free_lun, lun
  
  ; Done, so remove the report window
  envi_report_init, base = tranq, /finish

  nrs_close_shapes, [shapes]
end
