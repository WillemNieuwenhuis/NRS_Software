pro nrs_generate_coordmap, filename, dims = dims, map_info = map_info, lat = lat, lon = lon
  if n_elements(dims) ne 5 then begin
    ans = dialog_message('Dimensions of the new map are required')
    return
  endif
  if n_elements(map_info) eq 0 then begin
    ans = dialog_message('Coordinate system is required')
    return
  endif
  lat = n_elements(lat) ne 0 
  lon = n_elements(lon) ne 0 
  if lat + lon eq 0 then begin
    ans = dialog_message('You need to specify at least one of LAT or LON keywords')
    return
  endif

  ns = dims[2] - dims[1] + 1
  nl = dims[4] - dims[3] + 1
  
  image_lon = fltarr(ns, nl)
  envi_write_envi_file, image_lon, out_name = filename, dims = dims, map_info = map_info, r_fid = fid
  if lat eq 1 then image_lat = fltarr(ns, nl)
  
  for x = 0, ns - 1 do begin
    for y = 0, nl - 1 do begin
      envi_convert_file_coordinates, fid, x, y, xmap, ymap, /to_map
      if lat eq 1 then image_lat[x, y] = ymap
      if lon eq 1 then image_lon[x, y] = xmap
    endfor
  endfor

  envi_file_mng, id = fid, /remove
  
  ; write the updated image(s)
  if lat eq 1 then $
    envi_write_envi_file, image_lat, out_name = filename + '_lat', dims = dims, map_info = map_info, r_fid = fid
  if lon eq 1 then $
    envi_write_envi_file, image_lon, out_name = filename + '_lon', dims = dims, map_info = map_info, r_fid = fid
end

;Columns =  179
;Rows =  241
;Cell size =  1000 x 1000 m
;extents as corner of corner
;TOP =   9669312.0244
;LEFT =  114923.265436
;Right =  293923.265436
;Bottom= 9428312.0244
;Transverse Mercator
;Linear Unit =  Meter (1.000000)
;Angular Unit =  Degree (0.017453292519943299)
;False Easting =  500000
;False Northing = 10000000
;Central Meridian =  39
;Scale factor =   0.9996
;Latitude of Origin =  0
;Datum =   Arc 1960
;Spheroid =   Clarke 1880
pro create_longitude_map
  dims = [-1, 0, 178, 0, 240]
  utm = envi_proj_create(/utm, datum = 'ARC-1960 mean', zone = 37, /south)
  mc = [0D, 0D, 114923.265436D, 9669312.0244D]
  ps = [1000D, 1000D]
  map_info = envi_map_info_create(proj = utm, mc = mc, ps = ps)
  
  nrs_generate_coordmap, 'D:\data\NRS education\Claudia\longitude map\map', dims = dims, map_info = map_info, /lat, /lon
end