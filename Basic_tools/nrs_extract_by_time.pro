function nrs_datetime_from_string, date_str, time_str
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
;    as the timeseries image
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
;    prog_obj : in
;      Progress indicator object
;    cancelled :
;      If set indicates failure or user abort 
;
; :Author: nieuwenhuis
;-
pro nrs_extract_by_time, point_table, image $
                       , outname = outname $
                       , start_time, end_time $
                       , fieldname = fieldname $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  asc = nrs_read_csv(point_table, header = header, date_sep = '-/', time_sep = ':')
  sz = size(asc.field1, /dim)
  if n_elements(sz) ne 2 then return
  if sz[1] le 0 then return
  
  cancelled = 0
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  
  points = [transpose(asc.field1), transpose(asc.field2)]
  times = nrs_datetime_from_string(asc.field3, asc,field4)
  
  ; calculate time_mapping to timeseries bands
  sp = nrs_str2julian(start_time)
  ep = nrs_str2julian(end_time)
  tim_ix = fix(nb * (times - sp) / (ep - sp)) < (nb - 1)  
  
  nrs_set_progress_property, prog_obj, /start, title = 'Extracting values by time'
  num_ent = n_elements(points)
  vals = make_array(num_ent, type = dt)
  for i = 0, num_ent - 1 do begin
    if nrs_update_progress(prog_obj, i, num_ent, cancelled = cancelled) then return
    
    ; get the coordinate
    coordX = points[0, i]
    coordY = points[1, i]
    envi_convert_file_coordinates, fid_img, pixelX, pixelY, coordX, coordY
    if nrs_check_bounds(pixelX, pixelY, nl, ns) eq -1 then continue

    vals[i] = envi_get_slice(fid = fid, line = pixelY, xs = pixelX, xe = pixelX, pos = [tim_ix[i]])
  endfor
  
  ext = nrs_get_file_extension(image)
  if n_elements(fieldname) eq 0 then fieldname = file_basename(image, ext)
  bn = fieldname + '_bandnr'
  new_head = [header, bn, fieldname]
  outdata = asc
  outdata = create_struct('band', tim_ix, 'val', vals)
  if n_elements(outname) eq 0 then outname = point_table
  write_csv, outname, header = header, outdata
end

