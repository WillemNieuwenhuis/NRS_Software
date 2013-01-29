;+
; :Description:
;    Get the extension of a filename. The extension is the
;    part of the filename after the last '.' in the filename
;
; :Params:
;    name : in
;      Filename
;
; :Author: nieuwenhuis
;-
function nrs_get_file_extension, name
  pos = strpos(name, '.', /reverse_search)
  ext = ''
  if pos gt 0 then begin
    ext = strmid(name, pos)
  endif
  
  return, ext
end
