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
  stats = fltarr(4, num_files)
  header = 'filename,min,max,mean,stdev'
  fnames = strarr(num_files)
  valid_cnt = 0
  for f = 0, num_files - 1 do begin
    if nrs_update_progress(prog_obj, f, num_files, cancelled = cancelled) then return
    
    envi_open_file, files[f], r_fid = fid, /no_realize, /no_interactive_query
    if fid eq -1 then continue
    
    valid_cnt++
    envi_file_query, fid, fname = fname, dims = dims, data_ignore_value = undef
    if undef eq 1.0e-34 then void = temporary(undef)
    do_mask = (n_elements(undef) gt 0)
    if do_mask then begin
      ix = where(data eq undef, cnt)
      if cnt gt 0 then data[ix] = !values.f_nan
    endif
    
    data = envi_get_data(fid = fid, dims = dims, pos = 0)
    stats[0, f] = min(data, max = maxval, /nan)
    stats[1, f] = maxval
    stats[2, f] = mean(data, /nan)
    stats[3, f] = stddev(data, /nan)
    fnames[f] = file_basename(fname)
  endfor
  
  ;if n_elements(outtable) eq 0 then outtable = getOutname
  
  openw, lun, outtable, /get_lun

  printf, lun, header
  stats_str = string(stats, format = '(f0.5)')
  stats_str = reform(stats_str, 4, valid_cnt, /overwrite)
  for l = 0, valid_cnt - 1 do begin
    printf, lun, strjoin([fnames[l], stats_str[*, l]], ',')
  endfor

  close, lun
  free_lun, lun
end
