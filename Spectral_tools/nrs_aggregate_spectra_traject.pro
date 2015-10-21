;+
; :description:
;    Calculate an aggregated spatial profile for the input track in the timeseries image.
;    The input track can have a single date per record, or a date range.
;    <p>
;    The output is a text table with the input track expanded with the image band value at that location.
;    The number of rows will be expanded as well in case multiple image bands fall in the date range 
;    of the input track.
;
; :params:
;    pnt_tbl : in, required
;      The input file with the points features (CSV only)
;    image : in, required
;      The input stack with the timeseries information
;    start_date : in, required
;      Date of the first band in the image
;    end_date : in, required
;     Date of the last band in the image
;
; :keywords:
;    aggr_func : in, optional, default = average
;      The function to use for the spatial aggregation; available functions:
;      - min
;      - max
;      - mean
;      - median
;    kernel : in, optional, default = 3
;      The size of the window (in pixels) around the location to include in the aggregation.
;      Kernel size can be [1 .. 11].
;    kern_type: in, optional, default = 0
;      The type of kernel area: 0 = square; 1 = circle
;      Note this will sample spectra around the central pixel, either as a square or as a circle,
;      but the distances are calculated assuming all pixel are the same size
;    colname_start : in, required
;      The name of the column in the input table to use as start date in the range
;    colname_end : in, optional
;      The name of the column in the input table to use as end date in the range. If
;      this is missing only a single date is used (in colname_start)
;    aggr_temporal : in, optional, default = no
;      In case of a date range: aggr_temporal is true (yes) then perform aggregation both spatial
;      and temporal, if aggr_temporal = false (no) only aggregate spatial.
;      The temporal aggregation function is always average (spatial aggregation is determined by aggr_func)
;    xytable : in, optional, default = no
;      If true (yes) then the output will be an XY table with one location and spectrum
;      per row. If false (no) then the table will be organised with one location
;      and spectrum per column.
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
;   - aug 2015 - created
;-
pro nrs_aggregate_spatial_traject, pnt_tbl, image $
        , start_date, end_date $
        , aggr_func = aggr_func $
        , kernel = kernel, kern_type = kern_type $
        , colname_start = colname_start, colname_end = colname_end, aggr_temporal = aggr_temporal $
        , xytable = xytable $
        , outname = outname $
        , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  if n_elements(kernel) eq 0 then kernel = 3 $
  else kernel = min([11, max([1, kernel])])  ; kernel = 1, 3, 5, 7, 9, 11
  kern2 = fix(kernel / 2)
  if n_elements(kern_type) eq 0 then kern_type = 0 ; square

  if n_elements(aggr_func) eq 0 then aggr_func = 'mean'
  aggr_functions = ['min', 'max', 'mean', 'median']
  aggr_ix = where(strlowcase(aggr_func) eq aggr_functions, cnt)
  if cnt eq 0 then begin
    void = error_message('Unsupported aggregation function')
    return
  endif

  if kernel eq 1 then aggr_ix = 99  ; just single profile in kernel
  
  if n_elements(colname_start) eq 0 then begin
    void = error_message('Date field name is required')
    return
  endif
  
  useDateRange = n_elements(colname_end) ne 0
  useTemporalAggr = keyword_set(aggr_temporal)
  attr_fields = [colname_start]
  if useDateRange then attr_fields = [attr_fields, colname_end]

  envi_open_file, image, r_fid = mapID, /no_realize, /no_interactive_query
  envi_file_query, mapID, nb = nb, nl = nl, ns = ns, data_ignore_value = undef
  mi = envi_get_map_info(fid = mapID, undefined = undef_csy)
  if undef_csy eq 1 then begin
    void = error_message('Spectral image has no coordinates')
    return
  endif
  
  nrs_read_points_csv, pnt_tbl, x, y, hint_geo = isGeo $
           , attr_fields = attr_fields, attr_value = attr, attr_valid = attr_valid
  if n_elements(x) eq 0 then begin
    void = error_message('No points found')
    return
  endif
  
  if n_elements(attr_valid) lt 1 then begin
    void = error_message('Missing dates')
    return
  endif

  cancelled = 0

  ; date range for image  
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  date_interval = (ed - sd) / nb
  nrs_get_dt_indices, [sd, ed], period = date_interval, julian_out = jo, indices = ind_out

  ; date columns from table
  ; determine the band indices in the image from the dates from the table
  ; for the start dates...
  start_jd = nrs_str2julian(attr_value[0, *])
  nrs_match_arrays, jo, start_jd, index = ixfirst
  if useDateRange then begin
    ; ... and optionally for the end dates
    end_jd = nrs_str2julian(attr_value[1, *])
    nrs_match_arrays, jo, end_jd, index = ixlast
  endif

  pointCount = n_elements(x)  ; input points

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

  ; determine number of output records
  ; and reserve space
  nrRecOut = pointCount   ; single date
  if useDateRange then begin
    nrRecOut = total(ixlast - ixfirst)
  endif
  profiles = fltarr(4, nrRecOut)
  rec_count = 0

  nrs_aggregate_spectra_select, kernel, kern_type, select = select
  index = [0, reform(total(select[0, *], /cum, /preserve), kernel)]
  nrs_set_progress_property, prog_obj, /start, title = 'Aggregate spectrum profile'
  for i = 0, pointCount - 1 do begin
    if nrs_update_progress(prog_obj, i, pointCount, cancelled = cancelled) then return

    ; only points located inside the bounds of the image can be processed
    if nrs_check_bounds(pixelX[i] - kern2, pixelY[i] - kern2, nl, ns) eq 0 then continue
    if nrs_check_bounds(pixelX[i] + kern2, pixelY[i] + kern2, nl, ns) eq 0 then continue

    ; collect the entire kernel
    lines = indgen(kernel) - kern2
    pos = [ixfirst[i]]
    if useDateRange then pos = indgen(ixlast[i] - ixfirst[i]) + ixfirst[i]
    date_count = n_elements(pos)
    specTotal = fltarr(total(select[0, *]), date_count)
    for l = 0, kernel - 1 do begin
      line = pixelY[i] + lines[l]
      xs = pixelX[i] + select[1, l]
      xe = pixelX[i] + select[2, l]
      specTotal[index[l] : index[l + 1] - 1, *] = envi_get_slice(fid = mapID, line = line, xs = xs, xe = xe, pos = pos /bil)
    endfor

    ; then spatial aggregation
    case aggr_ix of
      0  : prof = min(spectotal, dim = 1)
      1  : prof = max(spectotal, dim = 1)
      2  : prof = mean(spectotal, dim = 1)
      3  : prof = median(spectotal, dim = 1)
      99 : prof = spectotal   ; kernel size == 1, just copy
    endcase
    if useDateRange then begin
      profiles[3, rec_count : rec_count + date_count] = prof
      rec_count += date_count
    endif else begin
      profiles[3, rec_count] = mean(prof, dim = 1)
      rec_count++
    endelse
    
  endfor
  ix = where(valid_profiles eq 1, cnt)
  if cnt eq 0 then begin
    void = error_message('None of the points (including kernel) overlap image')
    return
  endif
  if keyword_set(xytable) then begin
    dig = fix(alog10(nb)) + 1
    form = '("band_",' + string(dig, format = '("i0", i0)') + ')'
    hdr = ['X', 'Y', string(indgen(nb) + 1, format = form)]
    profiles = [transpose(x[ix]), transpose(y[ix]), transpose(profiles[ix, *])]
  endif else begin
    hdr = string([transpose(x[ix]), transpose(y[ix])], format = '("(",f0.6,":",f0.6,")")')
    profiles = profiles[ix, *]
  endelse

  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_prof', ext = '.csv')

  write_csv, outname, header = hdr, profiles
end
