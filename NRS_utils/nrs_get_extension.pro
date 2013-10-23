;+
; :Description:
;    Get the extension of a filename. The extension is the
;    part of the filename after the last '.' in the filename. The '.'
;    is included in the extension unless the exclude_dot keyword is set.
;
; :returns:
;   The file extension
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
;-
function nrs_get_file_extension, name, exclude_dot = exclude_dot
  compile_opt idl2, logical_predicate
  
  pos = strpos(name, '.', /reverse_search)
  ext = ''
  if pos gt 0 then begin
    if keyword_set(exclude_dot) then pos = min([pos + 1, strlen(name)])
    ext = strmid(name, pos)
  endif
  
  return, ext
end
