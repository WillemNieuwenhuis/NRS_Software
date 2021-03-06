;+
; :Description:
;    Calculate stack statistics. Calculated are: Mean, Stddev, CV, Min, Max.
;    The statistics are calculated in the Z-direction (usually the time dimension)
;
; :Params:
;    stack : in, required
;      Full filename of the input stack
;
; :Keywords:
;    outname  : out, optional
;      Full name of the output statistics; if not specified the output name is derived from the input name
;    ignore_undef : in, optional, default = no
;      If set: ignore the data specified undef value during the calculation. Is overruled by ignore_value
;    ignore_value : in, optional
;      If specified: exclude this value from the calculation and overrule the data specified ignore_value
;    prog_obj : in, optional
;      A ProgressBar object to indicate progress
;    cancelled : out, optional
;      If set, the user stopped the process
;
; :Author: nieuwenhuis
;-
pro nrs_stack_statistics, stack, outname = outname $
                        , ignore_undef = ignore_undef $
                        , ignore_value = ignore_value $ 
                        , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  envi_open_file, stack, r_fid = fid, /no_realize, /no_interactive_query

  if fid eq -1 then begin
    void = error_message('Input reference stack could not be opened!', traceback = 0, /error)
    return
  endif

  nrs_set_progress_property, prog_obj, title = 'Calculate stack statistics', /start
  
  ignore_undef = keyword_set(ignore_undef)
  
  cancelled = 0
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then void = temporary(mi)

  ; deal with internal undef or user specified undef. User defined gets preference
  ; check internal undef
  chktype = size(undef, /type)
  no_undef = ((chktype eq 4) or (chktype eq 5)) and (undef eq 1e-34)
  ignore_undef = ignore_undef && ~no_undef
  ; User specified undef:
  hasIgnore = (n_elements(ignore_value) gt 0) && (strlen(ignore_value) gt 0)
  if hasIgnore then begin
    undef = (fix(ignore_value, type = dt, /print))[0]
    ignore_undef = 1
  endif

  if n_elements(outname) eq 0 then $
    outname = getoutname(fname, postfix = '_stat', ext = '.dat')
  openw, unit, outname, /get_lun
  
  out = make_array(ns, 5, type = dt)
  pos = indgen(nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    
    ; read a slice and convert to float for undef handling
    slice = float(envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos))   
    if ignore_undef then begin 
      uix = where(slice eq undef, cnt_undef)
      if cnt_undef gt 0 then begin
        slice[uix] = !values.f_nan
      endif
    endif
    res = moment(slice, dim = 2, /nan)
    out[*, 0] = res[*, 0]
    out[*, 1] = sqrt(res[*, 1])
    out[*, 2] = out[*, 1] / out[*, 0]  
    out[*, 3] = min(slice, dim = 2, max = mx, /nan)
    out[*, 4] = mx
    
    writeu, unit, out

  endfor
 
  bnames = ['Mean', 'Stddev', 'CV', 'Min', 'Max'] 
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  close, unit
  free_lun, unit
  envi_setup_head, fname = outname $
    , data_type = dt $
    , /write $
    , interleave = 1 $  ; BIL = 1
    , nb = 5, nl = nl, ns = ns $
    , bnames = bnames $
    , inherit = meta $
    , map_info = mi $
    , data_ignore_value = undef

end

pro nrs_aggregate_excoutliers, stack = stack, outname = outname $
    , aggr_method = aggr_method $ 
    , perc_low = perc_low, perc_high = perc_high $
    , exclude_outliers = exclude_outliers $
    , outlier_name = outlier_name $
    , start_date = start_date $
    , end_date = end_date $
    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  envi_open_file, stack, r_fid = fid, /no_realize, /no_interactive_query
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt $
    , fname = fname, bnames = bnames $
    , data_ignore_value = undef
  chktype = size(undef, /type)
  has_valid_undef = ~( ((chktype eq 4) or (chktype eq 5)) and (undef eq 1e-34) )
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)

  if strlowcase(aggr_method) eq 'all' then begin
    bnames = ['sum', 'min', 'max', 'mean', 'stdev', 'median']
    out = make_array(ns, nl, n_elements(bnames), type = dt)
    postfix = '_aggr'
  end else begin
    out = make_array(ns, nl, type = dt)
    bnames = [aggr_method]
    postfix = '_' + aggr_method
  endelse
  
  aggregator = NrsStackAggregate()
  aggregator->setproperty, perc_low = perc_low, perc_high = perc_high $
    , base_start_date = start_date $
    , base_end_date = end_date $
    , use_percentiles = exclude_outliers $
    , outlier_name = outlier_name $
    , stack_name = stack $
    , prog_obj = prog_obj

  if aggr_method eq 'All' then begin
    for bn = 0, n_elements(bnames) - 1 do begin
      method = bnames[bn]
      out[*, *, bn] = aggregator->aggregate(method = method)
      if bn eq 0 then aggregator->setproperty, outlier_name = ''  ; only need to report on outliers once 
    endfor
  endif else begin
    out[*] = aggregator->aggregate(method = aggr_method)
  endelse

  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = postfix, ext = '.dat')

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
; 
; :history:
;   - 2 May 2014: nieuwenhuis, Reduced memory footprint by reading slices
;-
pro nrs_aggregate_layers, fid, aggr_method, layers = layers, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  if fid eq -1 then return
  
  nrs_set_progress_property, prog_obj, title = 'Aggregate layers with ' + aggr_method, /start

  cancelled = 0

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt $
                 , fname = fname, bnames = bnames $
                 , data_ignore_value = undef
  chktype = size(undef, /type)
  has_valid_undef = ~( ((chktype eq 4) or (chktype eq 5)) and (undef eq 1e-34) )
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  
  if n_elements(layers) eq 0 then $
    layers = indgen(nb)
  nr_band = n_elements(layers)
  if aggr_method eq 'All' then begin
    out = make_array(ns, nl, 5, type = dt)
    bnames = ['Sum', 'Mean', 'Median', 'Min', 'Max']
    postfix = '_aggr'
  end else begin
    out = make_array(ns, nl, type = dt)
    bnames = [aggr_method]
    postfix = '_' + aggr_method
  endelse
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return

    cube = envi_get_slice(fid = fid, xs = 0, xe = ns - 1, line = l, pos = layers, /bil)
    if has_valid_undef then begin
      uix = where(cube eq undef, cnt_undef)
      if cnt_undef gt 0 then begin
        cube[uix] = !values.f_nan
      endif
    endif
  
    mn = min(cube, dimension = 2, /nan, max = mx)
    tot = total(cube, 2, /nan)
    avg = mean(cube, dim = 2, /nan)
    med = median(cube, dimension = 2)
    uxi = where(finite(mn, /nan), uxi_cnt)  ; find the locations with no data
    if uxi_cnt gt 0 then begin
      tot[uxi] = undef
      avg[uxi] = undef
      med[uxi] = undef
      mn[uxi] = undef
      mx[uxi] = undef
    endif
    if aggr_method eq 'All' then begin
      out[*, l, 0] = tot
      out[*, l, 1] = avg
      out[*, l, 2] = med
      out[*, l, 3] = mn
      out[*, l, 4] = mx
    end else begin
      case strlowcase(aggr_method) of
        'sum'    : out[*, l] = tot
        'mean'   : out[*, l] = avg
        'median' : out[*, l] = med
        'min'    : out[*, l] = mn
        'max'    : out[*, l] = mx
      endcase
    endelse
  endfor

  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = postfix, ext = '.dat')
    
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames, data_ignore_value = undef
end

;pro qqq
;  fn = 'E:\NRS\Lin Lin Li\aggregation_with_percentile\gemid_outlier.dat'
;  e = envi()
;  raster = e.openRaster(fn)
;  nb = raster.
;  data = raster.GetData(BANDS=[0], SUB_RECT=[100,449,550,899])
;end