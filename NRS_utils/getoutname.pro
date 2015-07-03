;+
;
; :Description:
;   Generate an output name based on a pattern and a postfix. The pattern is a filename
;   with or without a path. It should point to an existing folder if a path is included.
;   The prefix is inserted before the filename pattern (after the path), where any extension is preserved.
;   The postfix is added as a postfix to this pattern, where any extension is preserved.
;   The postfix is optional, in which case it defaults to '_out' 
; 
; :Params: 
;   pattern
;     the filename pattern
; 
; :Keywords:
;   basename : in, optional
;     If specified, replaces the filename part with basename
;   prefix : in, optional, default = ''
;     A text that is prepended to the filename part of the pattern, preserving any extension
;   postfix : in, optional, default = '_out'
;     A text that is appended to the filename part of the pattern, preserving any extension
;   ext : in, optional
;     A new extension for the file; it should include the dot
; 
; :Author:
;   Willem Nieuwenhuis
; 
; :History:
;   Change history::
;     november 2009: WN, created
;     july 2010: WN, Added basename and ext keywords
;     june 2011: WN, Added prefix keyword
;-
function getOutname, pattern, basename = basename, prefix = prefix, postfix = postfix, ext = new_ext
  compile_opt idl2, logical_predicate
  
  if n_elements(prefix) eq 0 then prefix = ''
  if n_elements(postfix) eq 0 then postfix = '_out'
  path = file_dirname(pattern)
  name = file_basename(pattern)
  name_bk = name
  if n_elements(basename) ne 0 then name = basename
  pos = strpos(name, '.', /reverse_search)
  if pos lt 0 then begin
    pos = strpos(name_bk, '.', /reverse_search)  ; use original extension
    if n_elements(new_ext) eq 0 then new_ext = strmid(name_bk, pos)
    pos = -1
  endif
  ext = ''
  if pos gt 0 then begin
    ext = strmid(name, pos)
    if strlen(ext) lt 6 then $
      name = strmid(name, 0, pos)
  endif
  if keyword_set(new_ext) then ext = strtrim(new_ext)
  
  result = prefix + name + postfix + ext
  if path ne '.' then result = path + path_sep() + result
   
  return, result 
end
