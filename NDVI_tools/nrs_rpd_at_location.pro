;+
; :description:
;    Calculate the index where the input reaches a certain percentage for the first time.
;    The input is a timeseries image stack. The calculation is done on a list
;    of coordinates (lat, lon).
;    <br>
;    The default case (window is not specified) only the actual point location is examined.
;    For window sizes from 3 up, as square with sides of the window size is put around
;    the location and all profiles are examined and aggregated.
;    <p>
;    The output is a new table copying the data from the input and adding a third column
;    with the index of the first value higher than the threshold.
;
; :params:
;    image : in, required
;      Timeseries input
;    table : in, required
;      CSV table with a list of locations (2 columns: lat, lon)
;
; :keywords:
;    perc : in, optional, default = 50 (%)
;      The value determining the threshold
;    out_name : in, optional
;      Specify the output name of the table
;    window : in, optional
;      The size of the square area around the image location to aggregate
;      profiles
;    winter_corr : in, optional
;      If set check if the values in the winter time (northern hemisphere) are higher
;      than summer values; if so the profile for the tested location is skipped.
;    cancelled : out
;      indicate error or user abort if set
;    prog_obj : in
;      Progressbar object for progress indication, can be NULL
;      in which case no progress is displayed
;
; :author: nieuwenhuis
; :history:
;   <li>4 june 2013: created 
;-
pro nrs_rpd_at_location, image, table, perc = perc $
                       , out_name = out_name $
                       , window = window $
                       , winter_corr = winter_corr $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  if n_elements(perc) eq 0 then perc = 0.5
  perc = max([0, min([perc / 100.0, 1])])
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid lt 0 then return
  
  fi = file_info(table)
  if fi.exists eq 0 then return

  use_window = (n_elements(window) gt 0) && (window ge 3)
  winter_corr = keyword_set(winter_corr)
   
  cancelled = 0
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  line_data = nrs_read_table(table, col_count = field_count, header = header)

  ; expect 2 columns: 1st = lat / lon, 2nd = lon / lat
  nrrec = n_elements(line_data[0, *])
  
  lat_pos = strpos(header, 'lat')
  lon_pos = strpos(header, 'lon')
  
  lat_ix = (lat_pos gt lon_pos)
  lon_ix = (lat_pos lt lon_pos)
  
  pos = indgen(nb)
  x = line_data[lon_ix, *]
  y = line_data[lat_ix, *]

  mi = envi_get_map_info(fid = fid, undef = undef)
  if undef eq 0 && mi.proj.type ne 0 then begin
    geo = envi_proj_create(/geographic) 
    envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
    envi_convert_file_coordinates, fid, pixelX, pixelY, crd_x, crd_y
  endif else begin
    envi_convert_file_coordinates, fid, pixelX, pixelY, x, y
  endelse
  
  out_data = make_array(3, nrrec, type = size(line_data, /type))
  out_data[0:1,*] = line_data
  if use_window then begin
    if winter_corr then begin
      leap = (nb eq 366)
      yy = 2007 + leap  ; 2007: can be any non-leap year
      sd = julday(1, 1, yy)
      ed = julday(12, 31, yy)
      dpp = nrs_get_period_from_range(sd, ed, nb)
      sum_s = (julday(3, 1, yy) - sd) / dpp + 1
      sum_e = (julday(11, 1, yy) - sd) / dpp - 1
      summer = intarr(nb)
      summer[sum_s : sum_e] = 1
      summer = rebin(summer, nb, window, window)
      summer = transpose(summer)
      winter = 1 - summer
    endif
    cube = make_array(window, window, nb)
    for p = 0, nrrec - 1 do begin
      w2 = window / 2
      for w = -w2, w2 do begin
        cube[*, w + w2, *] = envi_get_slice(fid = fid, pos = pos, line = pixelY[p] $
                , xs = pixelX[p] - w2, xe = pixelX[p] + w2)
      endfor
      if winter_corr then begin
        sum_max = max(cube*summer, dim = 3)
        win_max = max(cube*winter, dim = 3)
        mask = sum_max lt win_max
        cube[mask, *] = 0
        profile = mean(mean(cube, dim = 1), dim = 1)
      endif
      zp = where(profile ge perc, cnt_zp)
      out_data[2, p] = cnt_zp eq 0 ? -1 : zp[0] + 1
    endfor
  endif else begin
    for p = 0, nrrec - 1 do begin
      profile = envi_get_slice(fid = fid, pos = pos, line = pixelY[p], xs = pixelX[p], xe = pixelX[p])
      zp = where(profile ge perc, cnt_zp)
      out_data[2, p] = cnt_zp eq 0 ? -1 : zp[0] + 1 
    endfor
  endelse
  
  if (nb eq 365) || (nb eq 366) then out_header = header + ',doy' $
  else out_header = header + ',index'
  
  if n_elements(out_name) eq 0 then $
    out_name = getOutname(table, postfix = '_gwi', ext = '.csv')
  
  format = '(f13.7,",",f13.7,",",i03)'
  openw, unit, out_name, /get_lun
  printf, unit, out_header
  for r = 0, nrrec - 1 do begin
    printf, unit, out_data[*, r], format = format
  endfor
  close, unit
  free_lun, unit
  
  ans = dialog_message('Threshold GWI finished.', /info)
end