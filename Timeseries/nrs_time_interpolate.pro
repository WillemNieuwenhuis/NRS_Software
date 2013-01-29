pro nrs_time_interpolate, image, outname = outname $
                   , start_date, end_date $
                   , interval $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Could not open image', traceback = 0)
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb $
                      , bnames = bnames, data_type = dt, data_ignore_value = nodata
  mi = envi_get_map_info(fid = fid, undefined = undef_csy)
  if undef_csy eq 1 then void = temporary(mi)

  ; parameter checking
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif
  
  per_len = [1, 8, 10, 16, 30, 365]
  per_str = ['day', '8-day', '10-day', '16-day', 'month', 'year']
  input_period = (ed - sd + 1) / (nb - 1)   ; in days
  diff = abs(per_len - input_period)
  mn = min(diff, mn_ix)
  input_period = per_str[mn_ix]
  
  int_ix = where(per_str eq strlowcase(interval), int_cnt)
  if int_cnt eq 0 then begin
    ans = dialog_message('Unrecognised output period: ' + interval, title = 'Error', /error)
    return
  endif

  ; get the indices for the input image stack
  nrs_get_dt_indices, [sd, ed], period = input_period, julian_out = jul_in, /clip
  nrs_get_dt_indices, [sd, ed], period = input_period, julian_out = jul_range
  nrs_get_dt_indices, jul_range, period = interval, julian_out = jul_out

  if n_elements(outname) eq 0 then $
    outname = getOutname(image, postfix = '_tinp', ext = '.')

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  cancelled = 0

  nrs_set_progress_property, prog_obj, title = 'Time interpolation', /start

  nb_out = n_elements(jul_out)
  
  ; open the output for writing
  openw, unit, outname, /get_lun
  out_data = assoc(unit, make_array(ns, nb_out, type = dt))  ; bil

  out = make_array(ns, nb_out, type = dt)
  for l = 0, nl - 1 do begin
    if l mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    
    for s = 0, ns - 1 do begin
      out[s, *] = interpol(reform(data[s, *], nb, /overwrite), jul_in, jul_out)
    endfor
    
    out_data[l] = out
  endfor

 ; Now determine band names
  caldat, jul_out, mm, dd, yy
  doy = nrs_doy_from_julian(jul_out)
  bnames = string([transpose(yy), transpose(doy)], format = '("Year.Doy ",i04, ".", i03)')
  
  envi_setup_head, fname = outname $
          , data_type = dt $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nb_out, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi

  close, unit
  free_lun, unit  ; close assoc
end