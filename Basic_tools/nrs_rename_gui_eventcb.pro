function nrs_rename_files_getfiles, folder, extension
  compile_opt idl2, logical_predicate

  showfnames = ['']
  result = file_search(folder + "*" + extension, count = ncount)
  if ncount gt 0 then begin
    showfnames = file_basename(result)
  endif

  return, showfnames
end  

function nrs_rename_files_filter, files, filter, replace, in_list = in_list, out_list = dest
  compile_opt idl2, logical_predicate

  pos = strpos(files, filter)
  ix = where(pos ge 0, count)
  dest = ['']
  if count gt 0 then begin
    dest = (replace + strmid(files[ix], pos[ix] + strlen(filter)))[0,*]
  endif
  
  in_list = files[ix]
  
  return, dest
end

pro nrs_rename_files_fill_renamed_list, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_sources')
  widget_control, val_fld, get_uvalue = sources
  if n_elements(sources) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_prefix_orig')
  widget_control, val_fld, get_value = search
  search = search[0]

  can_go = (strlen(strtrim(search, 2)) ne 0) and (n_elements(sources) gt 0)
  go_button = widget_info(event.top, FIND_BY_UNAME = 'nrs_rename_files_gobutton')
  widget_control, go_button, sensitive = can_go

  dest = ['']
  if can_go then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_prefix_new')
    widget_control, val_fld, get_value = replace
    replace = replace[0]
  
    dest = nrs_rename_files_filter(sources, search, replace)
  endif

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_destinations')
  widget_control, val_fld, set_value = dest, set_uvalue = dest

end

pro nrs_rename_files_fill_filelist, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_source_folder')
  widget_control, val_fld, get_value = source
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_types_combobox')
  ftype = widget_info(val_fld, /combobox_gettext)

  if strlen(strtrim(source, 2)) gt 0 then begin
    showfnames = nrs_rename_files_getfiles(source, ftype)

    val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_sources')
    widget_control, val_fld, set_value = showfnames, set_uvalue = showfnames

    nrs_rename_files_fill_renamed_list, event
  endif
end

pro nrs_rename_files_handleok, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_source_folder')
  widget_control, val_fld, get_value = source_folder
  if n_elements(source_folder) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_dest_folder')
  widget_control, val_fld, get_value = dest_folder
  if n_elements(dest_folder) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_types_combobox')
  ftype = widget_info(val_fld, /combobox_gettext)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_prefix_orig')
  widget_control, val_fld, get_value = search
  search = search[0]

  val_fld = widget_info(event.top, find_by_uname = 'nrs_rename_files_prefix_new')
  widget_control, val_fld, get_value = replace
  replace = replace[0]

  if strlowcase(source_folder) eq strlowcase(dest_folder) then return
  
  in_files = nrs_rename_files_getfiles(source_folder, ftype)
  if n_elements(in_files) eq 0 then return
  
  dest = nrs_rename_files_filter(in_files, search, replace, in_list = in_list)

  for f = 0, n_elements(in_files) - 1 do begin
    inputfile = source_folder + in_list[f]
    outputfile = dest_folder + dest[f]
    file_copy, inputfile, outputfile, /overwrite
;    if keyword_set(verbose) then $
;      print, inputfile + ' --> ' + outputfile
  endfor
  
  void = dialog_message('Done, ' + string(n_elements(in_files), format = '(i0)') + ' files copied.' $
                        , /information)
end

