;+
; :Description:
;    Read the lines from a text file into a string array
;
; :Params:
;    filename : in, required
;      Input text file
;
; :Author: nieuwenhuis
; :History: Nov 28, 2012
;-
function nrs_read_listfile, filename
  if n_elements(filename) eq 0 then return, []
  
  openr, unit, filename, /get_lun, error = err
  if err ne 0 then begin
    ans = error_message('Error opening list file: ' + filename, /errror)
    return, []
  endif
  
  lst = strarr(file_lines(filename))
  readf, unit, lst
  close, unit
  free_lun, unit
  ix = where(strlen(strtrim(lst, 2)) gt 0, cnt)
  if cnt eq 0 then return, []
  
  return, lst[ix]
end
