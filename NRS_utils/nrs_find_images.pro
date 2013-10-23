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
                        , case_sens = case_sens, exclude_hdr = exclude_hdr
  compile_opt idl2, logical_predicate

  if ~keyword_set(ext) then ext = 'hdf'
  
  exclude_hdr = keyword_set(exclude_hdr)
  
  case_sens = (n_elements(case_sens) eq 0) || keyword_set(case_sens)
  patlist = strsplit(pattern, ',', /extract, count = pat_count)
  ; collect all image files
  folder = file_dirname(folder) + path_sep() + file_basename(folder)
  file_mask = folder + path_sep() + '*.' + ext
  raw_files = file_search(count = file_count, file_mask)

  if file_count le 0 then return, []
  
  ; filter out non-image files
  include = intarr(file_count)
  for i = 0, file_count - 1 do begin
    if exclude_hdr then begin
      if strlowcase(nrs_get_file_extension(raw_files[i])) eq '.hdr' then continue
    endif
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
