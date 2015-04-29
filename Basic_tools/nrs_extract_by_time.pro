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
  
  cancelled = 0
  
  entire = keyword_set(entire) || (nr_fields lt 4)
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  
  sp = nrs_str2julian(start_time)
  ep = nrs_str2julian(end_time)
  
  mi = envi_get_map_info(fid = fid, undef = undef_csy)
  if (undef_csy eq 0 && mi.proj.type ne 0) && hint_geo then begin
    geo = envi_proj_create(/geographic) 
    envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
    envi_convert_file_coordinates, fid, pixelX, pixelY, crd_x, crd_y
  endif else begin
    envi_convert_file_coordinates, fid, pixelX, pixelY, x, y
  endelse
  
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

    if entire then begin
      vals[i, *] = envi_get_slice(fid = fid, line = pixelY[i], xs = pixelX[i], xe = pixelX[i], pos = pos_ent)
    endif else begin
      vals[i] = envi_get_slice(fid = fid, line = pixelY[i], xs = pixelX[i], xe = pixelX[i], pos = [tim_ix[i]])
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

