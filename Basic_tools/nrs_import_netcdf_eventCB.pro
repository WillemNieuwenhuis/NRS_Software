pro nrs_import_netcdf_handleOK, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_input_folder')
  widget_control, fld, get_value = folder

  fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_outfolder')
  widget_control, fld, get_value = outfolder

  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15 $
                        , title = 'Importing netCDF files to ENVI')
  progressBar -> Start

  ; define inner progress indicator
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15 $
                   , title = 'Importing netCDF files to ENVI')
                   
  files = nrs_find_images(folder, '.*', extension = 'nc')
  nr_cancelled = 0
  nr_files = n_elements(files)
  if nr_files gt 0 && string(files[0]) ne '-1' then $
    for f = 0, nr_files - 1 do begin
      if nrs_update_progress(progressBar, f, nr_files) then return

      fname = outfolder + path_sep() + file_basename(files[f])
      outname = getOutname(fname[0], postfix = '_imp', ext = '.')
      nrs_nc_get_data, files[f], out_name = outname $
                     , prog_obj = prog_obj, cancelled = cancelled
      nr_cancelled += cancelled
    endfor

  if obj_valid(prog_obj) gt 0 then prog_obj->Destroy
  progressBar -> Destroy
end