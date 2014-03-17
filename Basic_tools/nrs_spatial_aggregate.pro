pro nrs_spatial_aggregate, image, outname = outname $
          , factor = factor, method = method $
          , extend = extend, crop = crop $
          , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims $
       , bnames = bnames $
       , data_type = dt, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then delvar, mi
  
  if n_elements(factor) eq 0 then factor = 2
  factor = fix(factor)

  all_methods = ['sum', 'mean', 'min', 'max']
  if n_elements(method) eq 0 then method = 'mean'
  meth_ix = where(method eq all_methods, cnt)
  if cnt eq 0 then begin
    ans = dialog_message('Unsupported aggregation method', title = 'Information', /information)
    return
  endif
  
  do_extend = keyword_set(extend)
  do_crop = keyword_set(crop)
  do_extend = ~(do_extend || do_crop) + do_extend
  do_crop = ~do_extend 
  
  cancelled = 0
  
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_sag', ext = '.dat')
  
  nrs_set_progress_property, prog_obj, /start, title = 'Calculating spatial aggregation'

  ecx = (ns mod factor) ne 0
  ecy = (nl mod factor) ne 0
  ns_out = ns / factor + (ecx ? do_extend : 0)
  nl_out = nl / factor + (ecy ? do_extend : 0)
  ns_off = ns - (ns_out * factor)
  nl_off = nl - (nl_out * factor)
  nsr = min([ns, ns + ns_off])
  nlr = min([nl, nl + nl_off])
  data = make_array(ns + ns_off, factor, nb, type = dt)
  data[*] = dt eq 4 ? !values.f_nan : !values.d_nan
  pos = indgen(nb)
  openw, unit, outname, /get_lun
  for l = 0, nlr - 1, factor do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
    endif
    
    for f = l, min([l + factor, nl]) - 1 do begin
      data[0 : nsr - 1, f - l, *] = envi_get_slice(fid = fid, line = f, xs = 0, xe = nsr - 1, pos = pos, /bil)
    endfor
    data = reform(data, factor, ns + ns_off, nb, /overwrite)
    ; aggregate in X (lon) direction
    case meth_ix of
      0 : d2 = total(data, 1, /nan)
      1 : d2 = mean(data, dim = 1, /nan)
      2 : d2 = min(data, dim = 1, /nan)
      3 : d2 = max(data, dim = 1, /nan)
    endcase
    ; return data to original dimensions
    data = reform(data, ns + ns_off, factor, nb, /overwrite)
    ; d2 now is 2-D: ns + ns_off x nb, with ns + ns_off = ns_out * factor
    d2 = reform(d2, ns_out, factor, nb, /overwrite)
    ; aggregate in Y (lat) direction
    case meth_ix of
      0 : d2 = total(d2, 2, /nan)
      1 : d2 = mean(d2, dim = 2, /nan)
      2 : d2 = min(d2, dim = 2, /nan)
      3 : d2 = max(d2, dim = 2, /nan)
    endcase
    ; d2 now again is 2-D: ns_out x nb
    writeu, unit, d2
      
  endfor

  meta = envi_set_inheritance(fid, dims, /full, /no_spatial)
  mi.ps *= factor
;  mi.mc[2:3] += mi.ps * (factor - 1) / 2 ; relocate the tiepoint
  
  close, unit
  free_lun, unit
  envi_setup_head, fname = outname $
    , data_type = size(gdd, /type) $
    , /write $
    , interleave = 1 $  ; BIL
    , nb = nb, nl = nl_out, ns = ns_out $
    , bnames = bnames $
    , inherit = meta $
    , map_info = mi

end

