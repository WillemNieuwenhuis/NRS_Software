function nrs_datetime_from_string, date_str, time_str
  compile_opt idl2, logical_predicate

  nr_dates = n_elements(date_str)
  dt_arr = intarr(6, nr_dates)
  if n_elements(time_str) eq 0 then time_str = strarr(nr_dates) + '12:00:00'
  for i = 0, nr_dates - 1 do begin
    dt_arr[*, i] = nrs_split_datetime(date_str[i], time_str[i])
  endfor
  
  dmy = nrs_dmy_mapping(dt_arr)

  return, reform(julday(dt_arr[dmy[1], *], dt_arr[dmy[0], *], dt_arr[dmy[2], *] $
                      , dt_arr[3, *], dt_arr[4, *], dt_arr[5, *]) $
                 , nr_dates)
end

function nrs_select_buffer_subpix, x, y, px, py, xlow, ylow, xhigh, yhigh, distance
  compile_opt idl2, logical_predicate

  ; find out direction of y-coordinates: 1 = bottom to top, -1 = top to bottom
  ; assuming x-coordinates are from left to right
  dir = (yhigh gt ylow) * 2 - 1
  sel = [px, py]
  dlx = abs(x - xlow)
  dhx = abs(x - xhigh)
  dly = abs(y - ylow)
  dhy = abs(y - yhigh)
  ; check 4 connected first (these are always closer than the 8 connected)
  if dlx lt distance then sel = [sel, px - 1, py]
  if dhx lt distance then sel = [sel, px + 1, py]
  if dly lt distance then sel = [sel, px, py + dir]
  if dhy lt distance then sel = [sel, px, py - dir]

  ; only check the 8 connected if necessary, that is if at
  ; least one of the 4 connected neigboring pixels is included
  if n_elements(sel) gt 1 then begin  
    dll = (x - xlow) ^ 2 + (y - ylow) ^ 2
    dhl = (x - xhigh) ^ 2 + (y - ylow) ^ 2
    dlh = (x - xlow) ^ 2 + (y - yhigh) ^ 2
    dhh = (x - xhigh) ^ 2 + (y - yhigh) ^ 2
    dist2 = distance ^ 2
    if dll lt dist2 then sel = [sel, px - 1, py + dir]
    if dhl lt dist2 then sel = [sel, px + 1, py + dir]
    if dlh lt dist2 then sel = [sel, px - 1, py - dir]
    if dhy lt dist2 then sel = [sel, px + 1, py - dir]
  endif
  
  sel = reform(sel, 2, n_elements(sel) / 2, /over)
  return, sel
end

;+
; :description:
;    Calculate a circular buffer around a single point location. Returns an 2D array with
;    the x and y pixel coordinates of all pixel within a distance from this location.
;    The routine uses subpixel accuracy.
;    <p>
;    The x, y and distance units are assumed to be the same
;    
; :returns:
;   An 2D-array, (dimensions: 2 x number of pixels)
;   Col 0 contains all x pixel coordinates
;   Col 1 contains all y pixel coordinates
;
; :params:
;    fid : in, required
;      The ENVI file identifier of the image
;    x : in, required
;      The x coordinate of the location (can be world or geographic)
;    y : in, required
;      The y coordinate of the location (can be world or geographic)
;    distance : in, required
;      The distance to be used as buffer
;
; :keywords:
;   ps : in, optional
;     Pixel size in world coordinates, can either be a scalar (square pixels) or
;     a 2D-array (rectangular pixels). If specified allows special handling of
;     buffer sizes smaller than the pixel size.
;
; :history:
;   - oct, 2015 : created
;
; :author: nieuwenhuis
;-
function nrs_select_buffer, fid, x, y, distance, ps = ps
  compile_opt idl2, logical_predicate

  if n_elements(ps) eq 0 then ps = [0.0, 0.0]
  ; calculate length of half-diagonal
  psd = sqrt(total((ps / 2) ^ 2)) ; assume we have rectangular pixels

  if distance le psd then begin
    ; buffer is smaller than pixel so use different check
    envi_convert_file_coordinates, fid, px, py, x, y
    ; find real world pixel extents
    px = long(px)
    py = long(py)
    envi_convert_file_coordinates, fid, [px, px + 1], [py, py + 1], xmm, ymm, /to_map
    return, nrs_select_buffer_subpix(x, y, px, py, xmm[0], ymm[0], xmm[1], ymm[1], distance)
  endif
  
  ; calculate the initial buffer using world coordinates
  xmin = x - distance
  xmax = x + distance
  ymin = y - distance
  ymax = y + distance
  
  ; convert to pixel coordinates
  envi_convert_file_coordinates, fid, px, py, [xmin,xmax], [ymin, ymax]
  ; adjust the pixel coordinates
  px = long(px)
  py = long(py)
  py = py[sort(py)]
  ; add extra boundary to catch all candidates
  px[0]--
  py[0]--
  px[1]++
  py[1]++
  ; build index arrays for x and y pixel coordinates
  x1 = lindgen(px[1] - px[0] + 1) + px[0]
  ix = rebin(x1, px[1] - px[0] + 1, py[1] - py[0] + 1)
  y1 = lindgen(py[1] - py[0] + 1) + py[0]
  iy = rebin(transpose(y1), px[1] - px[0] + 1, py[1] - py[0] + 1)

  ; calculate the distance of the bottom-right corner of each
  ; pixel to the location under inpection
  envi_convert_file_coordinates, fid, ix, iy, cx, cy, /to_map
  dist = (cx - x) ^ 2 + (cy - y) ^ 2
  dist2 = long(distance) ^ 2
  
  sel = []
  for i = 1, n_elements(x1) - 1 do begin
    for j = 1, n_elements(y1) - 1 do begin
      ; check distances at all 4 corners of the pixel with [i,j] as bottom right
      if min(dist[i-1:i,j-1:j]) le dist2 then sel = [sel, [ix[i-1, j-1], iy[i-1, j-1]]]
    endfor
  endfor
  if n_elements(sel) gt 0 then begin
    sel = reform(sel, 2, n_elements(sel) / 2, /over)
  endif
   
  return, sel
end

;+
; :Description:
;    Extract values from timeseries data on specific locations and times. The
;    coordinates of the locations are assumed to be in the same coordinate system
;    as the timeseries image, or can be geographic when the image has a projection
;
; :Params:
;    point_table : in
;      The CSV formatted table containing the locations and times
;    image : in
;      The image to extract the values from. 
;    start_time : in
;      The first date and time of the timeseries data
;    end_time : in
;      The last date and time of the timeseries data
;
; :Keywords:
;    outname : in
;      The output filename for the new table. If not specified the original
;      table will be updated instead
;    fieldname : in
;      Name of the column to use for the extracted values. If not specified the
;      timeseries filename will be used
;    entire : in, optional
;      If set export the entire timeseries per location
;    buffer_dist : in, optional
;      Distance in same coordinates as image. If specified will calculated a buffer
;      around each location and return the average value of all pixels within the buffer
;    prog_obj : in, optional
;      Progress indicator object
;    cancelled : out
;      If set indicates failure or user abort 
;
; :Author: nieuwenhuis
;-
pro nrs_extract_by_time, point_table, image $
                       , outname = outname $
                       , start_time, end_time $
                       , fieldname = fieldname $
                       , entire = entire $
                       , buffer_dist = buffer_dist $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  nrs_read_points_csv, point_table, x, y, data = asc $
                     , hint_geo = hint_geo $
                     , valid = valid, nr_cols = nr_fields, header = header
  if ~valid then begin
    void = error_message('No data found in file', /error)
    return
  endif
  
  doBuffer = n_elements(buffer_dist) gt 0
  
  cancelled = 0
  
  entire = keyword_set(entire) || (nr_fields lt 4)
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  
  sp = nrs_str2julian(start_time)
  ep = nrs_str2julian(end_time)
  
  ; calculate the pixel coordinates
  mi = envi_get_map_info(fid = fid, undef = undef_csy)
  if (undef_csy eq 0 && mi.proj[0].type ne 0) && hint_geo then begin
    ; point locations are in lat lon, project them to image coordinates
    geo = envi_proj_create(/geographic) 
    envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
    envi_convert_file_coordinates, fid, pixelX, pixelY, crd_x, crd_y
    x = crd_x
    y = crd_y
  endif else begin
    ; work in the system of the image (either geographic or projected)
    envi_convert_file_coordinates, fid, pixelX, pixelY, x, y
  endelse
  
  ; get pixel extents of all point locations
  px_low = long(pixelX)
  px_high = long(pixelX + 1)
  py_low = long(pixelY)
  py_high = long(pixelY + 1)
  envi_convert_file_coordinates, fid, px_low, py_low, ext_xlow, ext_ylow, /to_map
  envi_convert_file_coordinates, fid, px_high, py_high, ext_xhigh, ext_yhigh, /to_map
  
  nr_points = n_elements(x)
  if entire then begin
    msg_title = 'Extracting values'
    vals = make_array(nr_points, nb, type = dt)
    tim_ix = lindgen(nb) + 1
    field_names = string(tim_ix, format = '("band_",i0)')
    header = [header, field_names]
  endif else begin
    ; calculate time_mapping to timeseries bands
    times = nrs_datetime_from_string(asc.field3, asc.field4)
    tim_ix = fix(nb * (times - sp) / (ep - sp)) < (nb - 1)
    msg_title = 'Extracting values by time'
    vals = make_array(nr_points, type = dt)
    ; calc fieldnames
    ext = nrs_get_file_extension(image)
    if n_elements(fieldname) eq 0 then fieldname = file_basename(image, ext)
    bn = fieldname + '_bandnr'
    header = [header, bn, fieldname]
  endelse  
  
  nrs_set_progress_property, prog_obj, /start, title = msg_title
  pos_ent = indgen(nb)
  for i = 0, nr_points - 1 do begin
    if nrs_update_progress(prog_obj, i, nr_points, cancelled = cancelled) then return
    
    if nrs_check_bounds(pixelX[i], pixelY[i], nl, ns) eq 0 then continue
    
    if doBuffer then begin
      xy_bar = nrs_select_buffer(fid, x[i], y[i], buffer_dist, ps = mi.ps)
      xy_ar = nrs_remove_out_bounds(xy_bar, 0, nl, ns) 
      xyi = xy_ar[0,*] + xy_ar[1, *] + ns
      xys = xyi[sort(xyi)]
      xy_ar = array_indices([ns, nl], xys, /dim)  ; now sorted by Y-coordinate
      count = n_elements(xy_ar[0,*])
      vals[i, *] = 0
      for e = 0, count - 1 do begin
        if entire then begin
          vals[i, *] += envi_get_slice(fid = fid, line = xy_ar[1, i], xs = xy_ar[0, i], xe = xy_ar[0, i], pos = pos_ent)
        endif else begin
          vals[i] += envi_get_slice(fid = fid, line = xy_ar[1, i], xs = xy_ar[0, i], xe = xy_ar[0, i], pos = [tim_ix[i]])
        endelse
      endfor
      vals[i, *] /= count
    endif else begin
      if entire then begin
        vals[i, *] = envi_get_slice(fid = fid, line = pixelY[i], xs = pixelX[i], xe = pixelX[i], pos = pos_ent)
      endif else begin
        vals[i] = envi_get_slice(fid = fid, line = pixelY[i], xs = pixelX[i], xe = pixelX[i], pos = [tim_ix[i]])
      endelse
    endelse
  endfor
  
  if entire then begin
    outdata = asc
    for f = 0, n_elements(field_names) - 1 do begin
      outdata = create_struct(outdata, field_names[f], vals[*, f])
    endfor
  endif $
  else outdata = create_struct(asc, 'band', tim_ix + 1, 'val', vals)
  if n_elements(outname) eq 0 then outname = getoutname(point_table, postfix = '_ebt')
  write_csv, outname, header = header, outdata
end

