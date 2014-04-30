;+
; :Description:
;    Get the extension of a filename. The extension is the
;    part of the filename after the last '.' in the filename. The '.'
;    is included in the extension unless the exclude_dot keyword is set.
;    The function accepts a single filename or an array of filenames.
;
; :returns:
;   The file extension for each of the files in the input
;   
; :Params:
;    name : in
;      Filename
;
; :keywords:
;    exclude_dot : in, optional, default = 0 (no)
;      If set, remove the dot from the extension
;
; :Author: nieuwenhuis
; 
; :history:
;   Changes::
;     16 Dec 2011: nieuwenhuis, created
;     22 Oct 2013: nieuwenhuis, added exclude_dot keyword
;     30 Apr 2014: nieuwenhuis, name can now be array with filenames
;-
function nrs_get_file_extension, name, exclude_dot = exclude_dot
  compile_opt idl2, logical_predicate
  
  ext = []
  if n_elements(name) gt 0 then begin
    ext = strarr(n_elements(name))
    pos = strpos(name, '.', /reverse_search)
    ix = where(pos gt 0, cnt)
    if cnt gt 0 then begin
      if keyword_set(exclude_dot) then pos = min([transpose(pos + 1), transpose(strlen(name))], dim = 1)
      ext = strmid(name, transpose(pos))
    endif
  endif
  
  return, ext
end

;+
; :description:
;    Remove the extension of a filename. The extension is the
;    part of the filename after the last '.' in the filename. The function accepts a single 
;    filename or an array of filenames.
;
; :returns:
;   The filename(s) without the extension
;
; :params:
;    name : in
;      Filename
;
; :author: nieuwenhuis
;
; :history:
;   Changes::
;     30 April 2014: nieuwenhuis, created
;-
function nrs_remove_file_extension, name
  compile_opt idl2, logical_predicate
  
  fn = name
  if n_elements(name) gt 0 then begin
    pos = strpos(name, '.', /reverse_search)
    ix = where(pos gt 0, cnt)
    if cnt gt 0 then begin
      fn[ix] = strmid(name[ix], 0, transpose(pos[ix]))
    endif
  endif
  
  return, fn
end
