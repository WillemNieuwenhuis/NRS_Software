function nrs_extract_lib_modules, names, paths
  idl_lib = !dir + path_sep() + 'lib'
  hook_lib = !dir + path_sep() + 'lib' + path_sep() + 'hook'
  r = paths
  n = names
  check = where(strpos(r, hook_lib) eq -1, cnt)
  if cnt gt 0 then begin
    r = r[check]
    n = n[check]
  endif
  check = where(strpos(r, idl_lib) ge 0, cnt)
  if cnt gt 0 then begin
    r = r[check]
    n = n[check]
  endif
  
  return, {name : n, source : r} 
end

;+
; :Description:
;    Get a list of user-written functions and procedures; this includes
;    the source they are compiled from (*.pro files), or loaded from (*.sav)
;    The system routines are recognized by the string 'ITT' in the source path
;
; :Returns:
;   A list of structures containing the name of the routine (name field) and the
;   source (source field)
;   
; :Keywords:
;    exclude_sav : in
;      If set, exclude those functions and procedures that are loaded from *.sav files
;    no_lib : in
;      If set do not include IDL library routines
;    full_src_path : in
;      If set, get the complete filepath of the sources
;    csv_file : in
;      The name of a csv file for persisting the list of routines  
;
; :Author: nieuwenhuis
;-
function nrs_find_my_routines, exclude_sav = exclude_sav $
                             , no_lib = no_lib $
                             , full_src_path = full_src_path, csv_file = csv_file $
                             , files = files
  compile_opt idl2, logical_predicate
  
  ; first procedures
  res = routine_info(/source)
  paths_pro = res.path
  names_pro = res.name

  ; now the functions
  res = routine_info(/source, /function)
  paths_fun = res.path
  names_fun = res.name

  if n_elements(paths_pro) + n_elements(paths_fun) eq 0 then return, []
  
  lst = [names_pro, names_fun]
  src = [paths_pro, paths_fun]
  ix = where(strlen(strtrim(src, 2)) gt 0, cnt)
  if cnt gt 0 then begin
    lst = lst[ix]
    src = src[ix]
  endif
  
  if keyword_set(exclude_sav) then begin
    ; exclude modules loaded from a sav file
    ix = where(strpos(src, '.sav') eq -1, count)
    if count gt 0 then begin
      lst = lst[ix]
      src = src[ix]   
    endif
  endif
  
  log_ix = []
  for f = 0, n_elements(files) - 1 do begin
    ix = where(files[f] eq file_basename(src), cnt)
    if cnt gt 0 then log_ix = [log_ix, ix]
  endfor
  
  if ~keyword_set(no_lib) then begin
    res = nrs_extract_lib_modules(lst, src)
    lst = [lst[log_ix], res.name]
    src = [src[log_ix], res.source]
  endif else begin
    lst = lst[log_ix]
    src = src[log_ix]
  endelse
  
  if n_elements(full_src_path) eq 0 then $
    src = file_basename(src)
  
  if n_elements(csv_file) gt 0 then begin
    openw, lun, csv_file, /GET_LUN
    printf, lun, 'Name;source'
    printf, lun, [transpose(lst), transpose(src)], format = '(a,";",a)'

    close, lun
    free_lun, lun
  endif
  
  return, {name : lst, source : src}
end