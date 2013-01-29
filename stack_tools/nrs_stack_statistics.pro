pro nrs_stack_statistics, fid, outname = outname $
                        , ignore_undef = ignore_undef $
                        , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  if fid eq -1 then return

  nrs_set_progress_property, prog_obj, title = 'Calculate stack statistics', /start
  
  ignore_undef = keyword_set(ignore_undef)
  
  cancelled = 0
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname, data_ignore_value = undef
  has_undef = (size(undef, /type) eq dt) or ( (size(undef, /type) eq 5) and undef ne 1e34)
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then void = temporary(mi)
  
  cube = make_array(ns, nl, nb, type = dt)
  out = make_array(ns, nl, 5, type = dt)
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

    cube[*, *, b] = envi_get_data(fid = fid, dims = dims, pos = b)
  endfor
  
  if ignore_undef then begin 
    uix = where(cube eq undef, cnt_undef)
    if cnt_undef gt 0 then begin
      cube[uix] = !values.f_nan
    endif
  endif
  res = moment(cube, dim = 3, /nan) ; only works in ENVI 4.8!
  out[*, *, 0] = res[*, *, 0]
  out[*, *, 1] = sqrt(res[*, *, 1])
  out[*, *, 2] = out[*, *, 1] / out[*, *, 0]  
  out[*, *, 3] = min(cube, dim = 3, max = mx, /nan)
  out[*, *, 4] = mx
 
  bnames = ['Mean', 'Stddev', 'CV', 'Min', 'Max'] 
  
  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = '_stat', ext = '.')
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames, data_ignore_value = undef
end

;+
; :Description:
;    Aggregate layers in a stack. The user can define the indices of the layers to be aggregated.
;
; :Params:
;    fid : in, required
;      The file handle of the layer stack
;    aggr_method : in, required
;      The aggregation method. If 'All' is specified as method then all methods are calculated. The
;      available methods are : 'Sum', 'Mean', 'Median', 'Min', 'Max'
;
; :Keywords:
;    layers : in, optional
;      The indices of the layers to be aggregated
;    outname : in/out, optional
;      The name of the output aggregation; default the name is calculated from the input,
;      and the name of the aggregation method is appended to the input name
;    prog_obj : in, optional
;      A progressBar object to be used to display progress
;    cancelled : out
;      Indicates if the process was interupted by the user in the progressBar
;
; :Author: nieuwenhuis
;-
pro nrs_aggregate_layers, fid, aggr_method, layers = layers, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  if fid eq -1 then return
  
  nrs_set_progress_property, prog_obj, title = 'Aggregate layers with ' + aggr_method, /start

  cancelled = 0

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = undef)
  
  if n_elements(layers) eq 0 then $
    layers = indgen(nb)
  nr_band = n_elements(layers)
  cube = make_array(ns, nl, nr_band, type = dt)
  for b = 0, nr_band - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

    cube[*, *, b] = envi_get_data(fid = fid, dims = dims, pos = layers[b])
  endfor
  
  if aggr_method eq 'All' then begin
    out = make_array(ns, nl, 5, type = dt)
    out[*, *, 0] = total(cube, 3)
    out[*, *, 1] = total(cube, 3) / nr_band
    out[*, *, 2] = median(cube, dimension = 3)
    out[*, *, 3] = min(cube, dimension = 3)
    out[*, *, 4] = max(cube, dimension = 3)
    bnames = ['Sum', 'Mean', 'Median', 'Min', 'Max']
    postfix = '_aggr'
  end else begin
    out = make_array(ns, nl, type = dt)
    case strlowcase(aggr_method) of
      'sum'    : out = total(cube, 3)
      'mean'   : out = total(cube, 3) / nr_band
      'median' : out = median(cube, dimension = 3)
      'min'    : out = min(cube, dimension = 3)
      'max'    : out = max(cube, dimension = 3)
    endcase
    bnames = [aggr_method]
    postfix = '_' + aggr_method
  endelse

  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = postfix, ext = '.')
    
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames
end

pro nrs_aggregate_to_period, fid, aggr_method $
;                           , start_date = start_date, period_in = period_in $
;                           , period_out = period_out $
                           , layers $
                           , outname = outname, bnames = bnames $
                           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  if fid eq -1 then return
  
  if layers[0] eq -1 then return
  
  nrs_set_progress_property, prog_obj, title = 'Aggregate layers with ' + aggr_method, /start

  cancelled = 0

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname
  mi = envi_get_map_info(fid = fid, undefined = undef)
  
  in_bands = max([layers, 0] - [0, layers])
  cube = make_array(ns, nl, in_bands, type = dt)
  out_cube = make_array(ns, nl, n_elements(layers), type = dt)
  sb = 0
  for gr = 0, n_elements(layers) - 1 do begin
    eb = layers[gr]
    for b = sb, eb - 1 do begin
      if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

      cube[*, *, (b - sb)] = envi_get_data(fid = fid, dims = dims, pos = layers[b])
    endfor
    
    case strlowcase(aggr_method) of
      'sum'    : out_cube[*, *, gr] = total(cube, 3)
      'mean'   : out_cube[*, *, gr] = total(cube, 3) / (eb - sb)
      'median' : out_cube[*, *, gr] = median(cube, dimension = 3)
      'min'    : out_cube[*, *, gr] = min(cube, dimension = 3)
      'max'    : out_cube[*, *, gr] = max(cube, dimension = 3)
    endcase
    
    sb = eb
  endfor
  postfix = '_' + aggr_method

  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = postfix, ext = '.')
    
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames
end