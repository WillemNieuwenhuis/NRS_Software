pro nrs_water_index_batch, images, water_index, outfolder = outfolder $
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
  
  if n_elements(outfolder) eq 0 then begin
    void = error_message('Output folder not specified', traceback = 0, /error)
    return
  endif

  cancelled = 0
  
  nrs_set_progress_property, prog_obj, /start, title = 'Batch water index'
  
  if use_listfile then begin
    files = nrs_read_listfile(images)
  endif
  
  if use_folder then begin
    files = nrs_find_images(images, '.*', extension = '*', /exclude_hdr)
  endif
  
  num_files = n_elements(files)
  for f = 0, num_files - 1 do begin
    if nrs_update_progress(prog_obj, f, num_files, cancelled = cancelled) then return
    
    envi_open_file, files[f], r_fid = fid, /no_realize, /no_interactive_query
    if fid eq -1 then continue

    outname = getOutname(outfolder + path_sep() + file_basename(files[f]), postfix = '_' + water_index)
    nrs_water_index, files[f], water_index, outname = outname
  endfor
  
end