function nrs_bias_expression, reeks, avg, threshold, func, bias = bias
  compile_opt idl2, hidden
  
  if func eq 1 then begin
    bias = (threshold - 100) / (avg - 100)
    reeks = (reeks - 100) * bias + 100
  endif
  
  if func eq 2 then $
    reeks *= threshold / avg
    
  return, (reeks < 100) > 0
end

;function nrs_bias_iteration, data, undef, limit, threshold, max_iterations, change_eps $
;      , corr_factor = corr_factor, corr_index = corr_index $
;      , out_data = out_data
;  compile_opt idl2, hidden
;
;  ; treat data with expression 1
;  for index = 0, n_elements(ix_lt) - 1 do begin
;    corrected = data[index, *]
;    avg = avg_all[index]
;    if avg eq 0 then continue
;    
;    bias = 1.0  ; default no correction
;    for iter = 0, max_iterations - 1 do begin
;      corrected = nrs_bias_expression(corrected, avg, threshold, 1, bias = bias) ; expression 1
;      
;      ; evaluate new average
;      avg_corr = mean(corrected)
;      if (abs(avg - avg_corr) lt change_eps) then break ; stop if change is small
;    endfor
;    out_data[index, *] = corrected
;    corr_factor[index, corr_index] = bias
;  endfor
;
;end

pro nrs_bias_per_season, data, undef, limit, threshold, max_iterations, change_eps $
          , corr_factor = corr_factor, corr_index = corr_index $
          , out_data = out_data
  compile_opt idl2, hidden

  avg_all = mean(data, dim = 2)
  ix_undef = where(avg_all eq undef, cnt_undef)
  ix_lt = where(avg_all lt limit, cnt_lt)
  ix_ge = where(avg_all ge limit, cnt_ge)
  if cnt_lt + cnt_ge eq 0 then return ; nothing to do
  
  corr_factor[*, corr_index] = undef  ; initialize

  ; treat data with expression 1  
  for index = 0, n_elements(ix_lt) - 1 do begin
    corrected = data[index, *]
    avg = avg_all[index]
    if avg eq 0 then continue
    
    bias = 1.0  ; default no correction
    for iter = 0, max_iterations - 1 do begin
      corrected = nrs_bias_expression(corrected, avg, threshold, 1, bias = bias) ; expression 1
      
      ; evaluate new average
      avg_corr = mean(corrected)
      if (abs(avg - avg_corr) lt change_eps) then break ; stop if change is small
    endfor
    out_data[index, *] = corrected
    corr_factor[index, corr_index] = bias
  endfor

  ; treat data with expression 2
  for index = 0, n_elements(ix_ge) - 1 do begin
    corrected = data[index, *]
    avg = avg_all[index]
    if avg eq 0 then continue
    
    bias = 1.0  ; default no correction
    for iter = 0, max_iterations - 1 do begin
      corrected = nrs_bias_expression(corrected, avg, threshold, 2, bias = bias) ; expression 2
      
      ; evaluate new average
      avg_corr = mean(corrected)
      if (abs(avg - avg_corr) lt change_eps) then break ; stop if change is small
    endfor
    out_data[index, *] = corrected
    corr_factor[index, corr_index] = bias
  endfor

end

pro nrs_apply_payment_bias, image $
             , max_iterations = max_iterations $
             , change_eps = change_eps $
             , threshold = threshold $
             , limit = limit $
             , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then  begin
    ans = dialog_message('Could not open timeseries', title = 'Error', /error)
    return
  endif
  
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns $
    , data_type = dt, bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = crd_undef)
  if crd_undef eq 1 then mi = temporary(dummy)

  ; handle nodata values
  if n_elements(undef) eq 0 then undef = 0
  if (size(undef, /type) eq 4) and (undef eq 1e-34) then undef = 255
  
  ; handle keyword parameter checking
  if n_elements(threshold) eq 0 then threshold = 15
  threshold = (threshold < 100) > 0

  if n_elements(limit) eq 0 then limit = 95
  limit = (limit < 100) > 0
  
  if n_elements(max_iterations) eq 0 then max_iterations = 1
  
  if n_elements(change_eps) eq 0 then change_eps = 1.0
  
  cancelled = 0

  outname = getoutname(image, postfix = '_bias', ext = '.dat')
  
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate and apply payment bias'

  season_early = make_array(ns, nl, nb / 2, type = dt)
  season_late = make_array(ns, nl, nb / 2, type = dt)
  for band = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, band, nb, cancelled = cancelled) then begin
      return
    endif
    
    if band mod 2 eq 0 then begin
      season_early[*, *, band / 2] = envi_get_data(fid = fid, dims = dims, pos = band)
    end else begin
      season_late[*, *, band / 2] = envi_get_data(fid = fid, dims = dims, pos = band)
    endelse
  endfor
  
  season_early = reform(season_early, ns * nl, nb / 2, /overwrite)
  season_late = reform(season_late, ns * nl, nb / 2, /overwrite)
  out_early = make_array(ns * nl, nb / 2, type = dt) + undef
  out_late = make_array(ns * nl, nb / 2, type = dt) + undef
  corr_factor = fltarr(ns * nl, 2)
  
  nrs_bias_per_season, float(season_early), undef, limit, threshold, max_iterations, change_eps $
          , corr_factor = corr_factor, corr_index = 0 $
          , out_data = out_early
  nrs_bias_per_season, float(season_late), undef, limit, threshold, max_iterations, change_eps $
          , corr_factor = corr_factor, corr_index = 1 $
          , out_data = out_late
  
  nrs_set_progress_property, prog_obj, title = 'Saving payment bias result'
  openw, unit, outname, /get_lun
  
  for band = 0, nb / 2 - 1 do begin
    writeu, unit, byte(out_early[*, band])
    writeu, unit, byte(out_late[*, band])
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  envi_setup_head, fname = outname $
    , data_type = dt $
    , /write $
    , interleave = 0 $  ; 0 == BSQ
    , nb = nb, nl = nl, ns = ns $
    , bnames = bnames $
    , data_ignore_value = undef $
    , map_info = mi

  close, unit
  free_lun, unit
  
  fn_bias = getoutname(image, postfix = '_bfact', ext = '.dat')
  envi_write_envi_file, corr_factor $
    , out_name = fn_bias $
    , interleave = 0 $  ; 0 == BSQ
    , nb = 2, nl = nl, ns = ns $
    , bnames = ['Early season', 'Late season'] $
    , map_info = mi $
    , data_ignore_value = undef

end

