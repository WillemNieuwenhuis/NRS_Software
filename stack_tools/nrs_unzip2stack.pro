pro nrs_unzip2stack, folder, out_stack, mask = mask, img_mask = img_mask
  compile_opt idl2, logical_predicate
  
  if n_elements(out_stack) eq 0 then begin
    void = error_message('Output name for stack missing')
    return
  endif
  
  if n_elements(mask) eq 0 then mask = '.*' ; regular expression: all files
  zipfiles = nrs_find_images(folder, mask, extension = 'zip')
  
  if n_elements(zipfiles) eq 0 then begin
    void = error_message('No archive files found, nothing to do.')
    return
  endif

  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Stacking zip contents" $
                        , /fast_loop $
                        )

  nrs_set_progress_property, prog_obj, title = 'Unzip files', /start

  if n_elements(img_mask) eq 0 then img_mask = '*.*'
  ext = nrs_get_file_extension(img_mask, /exclude_dot)
  img_mask = getOutname(img_mask, postfix = '', ext = '.*')
  tempfolder = nrs_get_temporary_dir()
  foreach file, zipfiles, file_nr do begin
    if nrs_update_progress(prog_obj, file_nr, n_elements(zipfiles), cancelled = cancelled) then return
    void = nrs_unzip(file, outfolder = tempfolder, mask = img_mask, /flatten)
  endforeach
  
  ; find all ESRI Arcview grid images
  files = nrs_find_images(tempfolder, '.*', ext = ext, /exclude_hdr)
  files = files[sort(files)]
  
  nrs_set_progress_property, prog_obj, title = 'Import to ENVI format', /start
  ; import all images to ENVI format
  foreach file, files, file_nr do begin
    if nrs_update_progress(prog_obj, file_nr, n_elements(files), cancelled = cancelled) then return
    
    nrs_esrisc2envi, file
  endforeach
  
  ; stack the imported files
  nrs_stack_image, out_stack, list_file = files, prog_obj = prog_obj, cancelled = cancelled

  prog_obj->Destroy
  
  file_delete, tempfolder, /recursive, /allow_nonexistent, /quiet
end
