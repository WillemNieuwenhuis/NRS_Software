pro nrs_stack_reverse_batch, images, keep_bnames = keep_bnames, outfolder = outfolder $
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
  
  nrs_set_progress_property, prog_obj, xs = 600, ys = 100, /start, title = 'Batch reverse stack layers'
  
  if use_listfile then begin
    files = nrs_read_listfile(images)
  endif
  
  if use_folder then begin
    files = nrs_find_images(images, '.*', extension = '*', /exclude_hdr)
  endif
  
  prog_inner = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Reverse stack layers" $
    , /fast_loop $
    )
  nrs_set_progress_property, prog_inner, xs = 650, ys = 150

  num_files = n_elements(files)
  for f = 0, num_files - 1 do begin
    if nrs_update_progress(prog_obj, f, num_files, cancelled = cancelled) then return
    
    envi_open_file, files[f], r_fid = fid, /no_realize, /no_interactive_query
    if fid eq -1 then continue

    outname = getOutname(outfolder + path_sep() + file_basename(files[f]), postfix = '_rev')
    nrs_stack_reverse, files[f], keep_bnames = keep_bnames, outname = outname $
            , prog_obj = prog_innner, cancelled = cancelled
    
    envi_file_mng, id = fid, /remove ; close input file
  endfor
  
end