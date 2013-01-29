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
;     A new extension for the file
; 
; :Author:
;   Willem Nieuwenhuis
; 
; :History:
;   <li>june 2011: Added prefix keyword
;   <li>july 2010: Added basename and ext keywords
;   <li>november 2009: created
;-
function getOutname, pattern, basename = basename, prefix = prefix, postfix = postfix, ext = new_ext
  if n_elements(prefix) eq 0 then prefix = ''
  if n_elements(postfix) eq 0 then postfix = '_out'
  path = file_dirname(pattern)
  name = file_basename(pattern)
  if n_elements(basename) ne 0 then name = basename
  pos = strpos(name, '.', /reverse_search)
  ext = ''
  if pos gt 0 then begin
    ext = strmid(name, pos)
    if strlen(ext) lt 6 then $
      name = strmid(name, 0, pos)
  endif
  if keyword_set(new_ext) then ext = strtrim(new_ext)
  
  return, path + path_sep() + prefix + name + postfix + ext
end
