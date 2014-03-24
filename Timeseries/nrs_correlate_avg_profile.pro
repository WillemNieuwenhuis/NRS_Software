pro nrs_correlate_avg_profile, image, profile_table $
    , iterations = iterations $
    , select_perc = select_perc $   ; range = [0, 1]
    , start_date = start_date $
    , end_date = end_date $
    , from_date = from_date $
    , to_date = to_date $
    , from_band = from_band $
    , to_band = to_band $
    , ignore_NAN = ignore_NAN $
    , outname = outname $
    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  ; parameter checking
  if n_elements(iterations) eq 0 then iterations = 100
  if n_elements(select_perc) eq 0 then select_perc = 0.3
  
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef
  per = nrs_get_period_from_range(sd, ed, nb, per_str = input_period)
  
  asc = nrs_read_csv(profile_table, header = header, date_sep = '-/', time_sep = ':')
  field_count = n_tags(asc)
  if field_count lt 2 then begin
    void = error_message('Missing data from table: date and value column required')
    return
  endif
  
  dates = nrs_str2julian(asc.field1)
  if (dates[0] ne sd) || (dates[-1] ne ed) then begin
    void = error_message('Image start and end dates don''t match table dates')
    return
  end
  
  prof_data = asc.field2
  if n_elements(prof_data) ne nb then begin
    void = error_message('Number of table rows doesn''t match number of bands')
    return
  end
  
  if (n_elements(from_band) gt 0) && (n_elements(to_band) gt 0) then begin
    from_band = (from_band > 0) < (nb - 1)
  end_band = (to_band > 0) < (nb - 1)
  if (from_band ge to_band) then begin
    from_band = 0
    to_band = nb - 1
  endif
  endif else begin
    from_d = sd
    to_d = ed
    if (n_elements(from_date) gt 0) && (n_elements(to_date) gt 0) then begin
      t1 = nrs_str2julian(from_date)
      t2 = nrs_str2julian(to_date)
      if t2 gt t1 then begin
        from_d = t1
        to_d = t2
      endif
    endif
    from_band = nrs_jul_to_band(from_d, sd, ed, nb)
    to_band = nrs_jul_to_band(to_d, sd, ed, nb)
  endelse
  pos = lindgen(to_band - from_band + 1) + from_band
  
  perc_str = string(fix(select_perc * 100), format = '(i02)')
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_corr' + perc_str, ext = '.csv')
  
  ignore_NAN = keyword_set(ignore_NAN)
  ignore_undef = (dt eq size(undef, /type)) && (undef ne 1e-34) ; true if undef is defined
  nan = dt eq 4 ? !values.f_nan : !values.d_nan
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Iteration' $
    , xs = 600, ys = 400
  
  prog_inner = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Profile correlation" $
    , /fast_loop $
    )

  nrs_set_progress_property, prog_inner, /start $
    , xs = 650, ys = 500
  
  nrs_log_line, outname, 'iteration, correlation'
  ix_all = lonarr(nb)
  part_ix = long(ns * nl * select_perc)
  for iter = 0, iterations - 1 do begin
    if nrs_update_progress(prog_obj, iter, iterations, cancelled = cancelled) then return
    
    ; determine random locations
    fsel = randomu(seed, ns, nl, /uniform)
    six = sort(fsel)
    part_val = fsel[six[part_ix]] 
    select = where(fsel lt part_val, cnt)
    select = array_indices([ns, nl], select, /dim)
    
    ; collect data from the random selected locations into 2D array accu (locations x bands)
    accu = []
    for l = 0, nl - 1 do begin
      if nrs_update_progress(prog_inner, l, nl, cancelled = cancelled) then return
      
      data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
      sel = where(select[1,*] eq l, cnt_l)
      if cnt_l gt 0 then $
        accu = [accu, data[select[0, sel], *]]
    endfor
    
    ; mask data ignore value
    if ignore_undef then begin
      ix = where(accu eq undef, cnt)
      if cnt gt 0 then accu[ix] = nan
    endif
    
    ; calculate the average profile for the selected locations
    accu_prof = mean(accu, dim = 1, nan = ignore_NAN)
    if ignore_NAN then begin
      ix = where(~finite(accu_prof, /nan), cnt_nan)
    endif else ix = ix_all
    
    corr = correlate(accu_prof[ix], prof_data[ix])
    
    nrs_log_line, outname, /append, strjoin([ string(iter + 1, format = '(i5)') $
                                   , string(corr, format = '(f0.8)') $
                                   ] $
                                   , ',' $
                                 )
  endfor
  
  if obj_valid(prog_inner) then prog_inner->destroy

end
