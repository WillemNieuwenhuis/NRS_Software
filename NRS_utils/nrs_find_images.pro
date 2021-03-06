;+
;
; :Description:
; Find images in a folder; a pattern defines which files to select, where the type of the 
; images is determined by the optional extension (defaults to 'HDF')
; The pattern is a comma delimited list of strings to match the filename
;
; :Returns:
;   A list of files matching the pattern. If not matches are found returns en empty list ([])
;   
; :Params:
;   folder : in
;     The folder to search files in (non-recursive]
;   pattern : in
;     A (comma delimited list of) match patterns for the filenames; each of the patterns is
;      matched as a regular expression against the found filenames. Only files that have
;      positive match are selected.
; 
; :Keywords:
;   extension : in, default = 'HDF'
;     The extension of the files to search
;   case_sens : in, optional, default = true
;     Match the filename case sensitive against the pattern
;   exclude_hdr : in, optional, default = false
;     If set exclude files with extension HDR assuming these are only ENVI header files
;
; :Examples:
;   pattern = 'NDVI,VIQ'
;
; :Author:
;   Willem Nieuwenhuis
; 
; :History:
;   <li>july 2010, created
;   <li>may 2013, added case_sens, exclude_hdr keywords
;   
;-
function nrs_find_images, folder, pattern, extension = ext $
                        , no_extension = no_extension $
                        , case_sens = case_sens, exclude_hdr = exclude_hdr
  compile_opt idl2, logical_predicate

  if ~keyword_set(ext) then ext = 'hdf'
  if strpos(ext, '.') eq 0 then ext = strmid(ext, 1)
  
  exclude_hdr = keyword_set(exclude_hdr)
  
  case_sens = (n_elements(case_sens) eq 0) || keyword_set(case_sens)
  patlist = []
  pat_count = 0
  if (n_elements(pattern) gt 0) then if (strlen(strtrim(pattern, 2)) gt 0) then $
    patlist = strsplit(pattern, ',', /extract, count = pat_count)
    
  ; collect all image files
  folder = file_dirname(folder) + path_sep() + file_basename(folder)
  if keyword_set(no_extension) then $
    file_mask = folder + path_sep() + '*' $
  else $
    file_mask = folder + path_sep() + '*.' + ext
  raw_files = file_search(count = file_count, file_mask)

  if file_count le 0 then return, []
  
  ; filter out non-image files
  include = intarr(file_count)
  for i = 0, file_count - 1 do begin
    if exclude_hdr then begin
      if strlowcase(nrs_get_file_extension(raw_files[i])) eq '.hdr' then continue
    endif
    if keyword_set(no_extension) then begin
      if strlen(nrs_get_file_extension(raw_files[i])) gt 0 then continue
    endif
    if pat_count eq 0 then include[i] = 1
    for p = 0, pat_count - 1 do begin
      if stregex(raw_files[i], patlist[p], fold_case = ~case_sens) ge 0 then begin
        include[i] = 1
        break
      endif
    endfor
  endfor
  
  ix = where(include > 0, ixcnt)
  if ixcnt gt 0 then return, raw_files[ix]
    
  return, []
end

;+
; :description:
;    Given a header file of an ENVI image, find the filename of the ENVI datafile
;
; :params:
;    headerfile
;      The name of the ENVI header file
;
; :author:
;   Willem Nieuwenhuis
;
; :history:
;   - 30 April 2014: created
;-
function nrs_get_envi_datafilename, headerfile
  compile_opt idl2, logical_predicate
  
  file_mask = nrs_remove_file_extension(headerfile) + '.*'
  raw_files = file_search(count = file_count, file_mask)
  ix = where(nrs_get_file_extension(raw_files) ne '.hdr', cnt)
  if cnt gt 0 then raw_files = raw_files[ix]

  return, raw_files
end