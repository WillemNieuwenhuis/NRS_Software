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

  if n_elements(outname) eq 0 then $
    outname = getoutname(fname, postfix = '_stat', ext = '.dat')
  openw, unit, outname, /get_lun
  
  out = make_array(ns, 5, type = dt)
  pos = indgen(nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    slice = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos)   
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

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, fname = fname, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = undef)
  
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

    cube = envi_get_slice(fid = fid, xs = 0, xe = ns - 1, line = l, pos = layers[b], /bil)
  
    if aggr_method eq 'All' then begin
      out[*, l, 0] = total(cube, 2, /nan)
      out[*, l, 1] = total(cube, 2, /nan) / nr_band
      out[*, l, 2] = median(cube, dimension = 2)
      out[*, l, 3] = min(cube, dimension = 2, /nan)
      out[*, l, 4] = max(cube, dimension = 2, /nan)
    end else begin
      case strlowcase(aggr_method) of
        'sum'    : out[*, l] = total(cube, 2, /nan)
        'mean'   : out[*, l] = total(cube, 2, /nan) / nr_band
        'median' : out[*, l] = median(cube, dimension = 2)
        'min'    : out[*, l] = min(cube, dimension = 2, /nan)
        'max'    : out[*, l] = max(cube, dimension = 2, /nan)
      endcase
    endelse
  endfor

  if n_elements(outname) eq 0 then $
    outname = getOutname(fname, postfix = postfix, ext = '.dat')
    
  envi_write_envi_file, out, out_name = outname, map_info = mi, bnames = bnames
end
