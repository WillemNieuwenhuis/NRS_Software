;+
; :description:
;    Calculate an aggregated spectral profile for all locations in the input features
;
; :params:
;    pnt_tbl : in, required
;      The input file with the points features
;    image : in, required
;      The input stack with the spectral information
;
; :keywords:
;    aggr_func : in, optional, default = average
;      The function to use for the aggregation; only average is implemented
;    kernel : in, optional, default = 3
;      The size of the window (in pixels) around the location to include in the aggregation.
;      Kernel size can be [3 .. 7].
;    outname : out, optional
;      The name of the output table (CSV); if not specified will be
;      derived from the input image name
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :author: nieuwenhuis
; :history:
;   - jan 2014 - created
;-
pro nrs_aggregate_spectra, pnt_tbl, image $
                         , aggr_func = aggr_func $
                         , kernel = kernel $
                         , outname = outname $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  if n_elements(kernel) eq 0 then kernel = 3 $
  else kernel = min([7, max([3, kernel])])  ; kernel = 3,5,7
  kern2 = fix(kernel / 2)

  if n_elements(aggr_func) eq 0 then aggr_func = 'mean'
  aggr_functions = ['min', 'max', 'mean', 'median']
  aggr_ix = where(strlowcase(aggr_func) eq aggr_functions, cnt)
  if cnt eq 0 then begin
    void = error_message('Unsupported aggregation function')
    return
  endif
  
  envi_open_file, image, r_fid = mapID, /no_realize, /no_interactive_query
  envi_file_query, mapID, nb = nb, nl = nl, ns = ns, data_ignore_value = undef
  mi = envi_get_map_info(fid = mapID, undefined = undef_csy)
  if undef_csy eq 1 then begin
    void = error_message('Spectral image has no coordinates')
    return
  endif
  
  ext = nrs_get_file_extension(pnt_tbl)
  if strlowcase(ext) eq '.shp' then begin
    crd = nrs_read_shape_points(pnt_tbl)
    x = crd.x
    y = crd.y
  endif else begin
    nrs_read_points_csv, pnt_tbl, x, y, hint_geo = isGeo
  endelse
  
  pointCount = n_elements(x)

  if (undef_csy eq 0 && mi.proj.type ne 0) && isGeo then begin
    geo = envi_proj_create(/geographic)
    envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
    envi_convert_file_coordinates, mapID, pixelX, pixelY, crd_x, crd_y
  endif else begin
    ; assume the same coordinate system
    envi_convert_file_coordinates, mapID, pixelX, pixelY, x, y
  endelse
  pixelX = round(pixelX)
  pixelY = round(pixelY)

  profiles = fltarr(pointCount, nb)
  valid_profiles = bytarr(pointCount)
  nrs_set_progress_property, prog_obj, /start, title = 'Aggregate spectrum profile'
  for i = 0, pointCount - 1 do begin
    if nrs_update_progress(prog_obj, i, pointCount, cancelled = cancelled) then return

    ; only points located inside the bounds of the image can be processed
    if nrs_check_bounds(pixelX[i] - kern2, pixelY[i] - kern2, nl, ns) eq - 1 then continue
    if nrs_check_bounds(pixelX[i] + kern2, pixelY[i] + kern2, nl, ns) eq - 1 then continue

    ; collect the entire kernel
    specTotal = fltarr(kernel, kernel, nb)
    for line = pixelY[i] - kern2, pixelY[i] + kern2 do begin
      lp = line - (pixelY[i] - kern2)
      spectotal[*, lp, *] = envi_get_slice(fid = mapID, line = line, xs = pixelX[i] - kern2, xe = pixelX[i] + kern2, /bil)
    endfor
    
    spectotal = reform(spectotal, kernel * kernel, nb, /overwrite) 
    case aggr_ix of
      0 : profiles[i, *] = min(spectotal, dim = 1)
      1 : profiles[i, *] = max(spectotal, dim = 1)
      2 : profiles[i, *] = mean(spectotal, dim = 1)
      3 : profiles[i, *] = median(spectotal, dim = 1)
    endcase
    valid_profiles[i] = 1
  endfor
  ix = where(valid_profiles eq 1, cnt)
  if cnt eq 0 then begin
    void = error_message('None of the points (including kernel) overlap image')
    return
  endif
  profiles = profiles[ix, *]
  hdr = string([transpose(x), transpose(y)], format = '("(",f0.6,":",f0.6,")")')
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_prof', ext = '.csv')
  
  write_csv, outname, header = hdr, profiles
end
