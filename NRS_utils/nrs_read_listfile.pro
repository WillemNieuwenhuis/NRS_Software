;+
; :Description:
;    Read the lines from a text file into a string array. The file is closed after the read.
;    Empty lines from the text file are removed.
;
; :Params:
;    filename : in, required
;      Input text file
;
; :Keywords:
;    lines : in, optional
;      if set only read the first "lines" lines from the file
;
; :Author: nieuwenhuis
; :History:
;   Changes::
;     28 Nov 2012: nieuwenhuis, created
;     18 Oct 2013: nieuwenhuis, added lines keyword
;-
function nrs_read_listfile, filename, lines = lines
  if n_elements(filename) eq 0 then return, []
  
  openr, unit, filename, /get_lun, error = err
  if err ne 0 then begin
    ans = error_message('Error opening list file: ' + filename, /errror)
    return, []
  endif
 
  nr_lines = file_lines(filename)
  if n_elements(lines) gt 0 then nr_lines = min([lines, nr_lines])
  lst = strarr(nr_lines)
  readf, unit, lst
  close, unit
  free_lun, unit
  ix = where(strlen(strtrim(lst, 2)) gt 0, cnt)
  if cnt eq 0 then return, []
  
  return, lst[ix]
end
