;+
; :Description:
;   Create x and y coordinate array maps for an image.
;   A projected coordinate system will give projected coordinates unless the force_geographic keyword
;   is set. 
;   If the image has no projection the map coordinates result (can be either map or geographic)
;
; :Params:
;    filename :
;      The name of the image (or imagestack).
;
; :Keywords:
;    force_geographic : in, optional
;      Force lat / lon output in case of a projection
;    prog_obj : in, optional
;      Progress indicator object
;    cancelled :  out
;      Indicate user abort of the operation, or wrong input
;    x_filename : in
;      The name of the output image with the x coordinate values (or lon)
;    y_filename : in
;      The name of the output image with the y coordinate values (or lat)
;
; :Author: nieuwenhuis
;-
pro nrs_generate_LL_grid, filename, force_geographic = force_geographic $
                  , x_filename = x_filename, y_filename = y_filename $ 
                  , prog_obj = prog_obj, cancelled = cancelled

  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, filename, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Could not open image', title = 'Generate coordinate map', /error, /noname, traceback = 0)
    return
  endif
  
  mapinfo = envi_get_map_info(fid = fid, undefined = undef)
  if undef ne 0 then begin
    void = error_message('Image does not have a coordinate system', title = 'Generate coordinate map', /error, /noname, traceback = 0)
    return
  endif

  cancelled = 0
  
  envi_file_query, fid, dims = dims, nl = nl, ns = ns, data_type = dt
  projection = envi_get_projection(fid = fid)
  geographic = envi_proj_create(/geographic)
  applyProj = (projection.name ne 'Arbitrary') and keyword_set(force_geographic) 
  
  lat = dblarr(ns, nl)
  lon = dblarr(ns, nl)
  
  cols = lindgen(ns)
  line_pos = lonarr(ns)
  
  nrs_set_progress_property, prog_obj, /start, title = 'Creating coordinate maps'
  
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
  
    envi_convert_file_coordinates, fid, cols, line_pos, map_x, map_y, /to_map
    if applyProj then begin
      envi_convert_projection_coordinates, map_x, map_y, projection, crd_x, crd_y, geographic
      lat[*, line] = crd_y
      lon[*, line] = crd_x
    endif else begin
      lat[*, line] = map_y
      lon[*, line] = map_x
    endelse
    line_pos += 1
  endfor

  if n_elements(y_filename) gt 0 then $
    envi_write_envi_file, lat, out_name = y_filename, ns = ns, nl = nl, map_info = mapinfo
  if n_elements(x_filename) gt 0 then $
    envi_write_envi_file, lon, out_name = x_filename, ns = ns, nl = nl, map_info = mapinfo
end

