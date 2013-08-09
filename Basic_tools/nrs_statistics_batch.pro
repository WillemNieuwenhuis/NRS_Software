pro nrs_statistics_batch, images, outtable = outtable $
                  , use_listfile = use_listfile, use_folder = use_folder $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  use_listfile = keyword_set(use_listfile)
  use_folder = keyword_set(use_folder)
  
  if use_listfile && use_folder then begin
    void = error_message('Cannot specify both USE_LISTFILE and USE_FOLDER keywords', traceback = 0, /error)
    return
  endif

  fi = file_info(images)
  is_file = fi.exists && ~fi.directory
  is_folder = fi.exists && fi.directory
  if is_file && ~use_listfile then begin
    void = error_message('Trying to open a folder as a file', traceback = 0, /error)
    return
  endif
  if is_folder && ~use_folder then begin
    void = error_message('Trying to open a file as a folder', traceback = 0, /error)
    return
  endif
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Batch calculate statistics'
  
  if use_listfile then begin
    files = nrs_read_listfile(images)
  endif
  
  if use_folder then begin
    files = nrs_find_images(images, '.*', extension = '*', /exclude_hdr)
  endif
  
  num_files = n_elements(files)
  if num_files eq 0 then return
  
  stats = fltarr(4, num_files)
  header = 'filename,min,max,mean,stdev'
  nrs_log_line, outtable, header

  prog_inner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , xsize = 200, ysize = 15, title = "File statistics, pass 1" $
                        , level = 1 $
                        , /fast_loop $
                        )

  for f = 0, num_files - 1 do begin
    if nrs_update_progress(prog_obj, f, num_files, cancelled = cancelled) then return
    
    envi_open_file, files[f], r_fid = fid, /no_realize, /no_interactive_query
    if fid eq -1 then continue
    
    envi_file_query, fid, fname = fname, ns = ns, nl = nl, data_ignore_value = undef, data_type = dt
    if undef eq 1.0e-34 then void = temporary(undef)
    do_mask = (n_elements(undef) gt 0)
    
    nl = long(nl)
    ns = long(ns)
    if ns * nl lt 10000000L then step_size = nl $
    else step_size = fix(10000000L / ns, type = 3) + 1
    nr_steps = 1 + nl / step_size
  
    mm = nrs_minmax_from_datatype(dt)
    my_min = mm[1]
    my_max = mm[0]
    my_sum = 0.0D
    my_cnt = 0L

    nrs_set_progress_property, prog_inner, /start, title = 'File statistics, pass 1'
    
    ; pass 1; calculate min, max and mean
    for i = 0L, nr_steps - 1 do begin
      if nrs_update_progress(prog_inner, i, nr_steps * 2, cancelled = cancelled) then return
      
      s_y = i * step_size
      e_y = min([s_y + step_size - 1, nl - 1])
      dims = [-1, 0, ns - 1, s_y, e_y]
      data = envi_get_data(fid = fid, dims = dims, pos = 0)
      
      if do_mask then begin
        ix = where(data eq undef, cnt)
        if cnt gt 0 then data[ix] = !values.d_nan
      endif else cnt = 0

      my_min = min([my_min, min(data, max = maxval, /nan)])
      my_max = max([my_max, maxval])
      my_sum += total(data, /nan, /double)
      my_cnt += cnt
    endfor
    my_cnt = ns * nl - my_cnt

    my_mean = my_sum / my_cnt
    sub_tot = 0
     
    nrs_set_progress_property, prog_inner, /start, title = 'File statistics, pass 2'
    
    ; pass 2: calculate stddev
    for i = 0L, nr_steps - 1 do begin
      if nrs_update_progress(prog_inner, i + nr_steps, nr_steps * 2, cancelled = cancelled) then return

      s_y = i * step_size
      e_y = min([s_y + step_size - 1, nl - 1])
      dims = [-1, 0, ns - 1, s_y, e_y]
      data = envi_get_data(fid = fid, dims = dims, pos = 0)
    
      if do_mask then begin
        ix = where(data eq undef, cnt)
        if cnt gt 0 then data[ix] = !values.f_nan
      endif

      sub_tot += total((data - my_mean) ^ 2, /nan)
    endfor
    stats_str = string([my_min, my_max, my_mean, sqrt(sub_tot / (my_cnt - 1))], format = '(f0.5)')
    nrs_log_line, outtable,  strjoin([file_basename(files[f]), stats_str], ','), /append
    envi_file_mng, id = fid, /remove
  endfor
  
  prog_inner->Destroy
end
