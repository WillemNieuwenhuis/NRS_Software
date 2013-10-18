;+
; :Description:
;    Write an string array to a text file
;
; :Params:
;    filename : in, required
;      Input text file
;    lines : in, required
;      The string array to write to file
;
; :Author: nieuwenhuis
; :History: Oct 15, 2013
;-
pro nrs_write_listfile, filename, lines
  compile_opt idl2
  if n_elements(filename) eq 0 then return
  
  if n_elements(lines) eq 0 then return
  
  openw, unit, filename, /get_lun, error = err
  if err ne 0 then begin
    ans = error_message('Error creating list file: ' + filename, /errror)
    return
  endif

  for i = 0, n_elements(lines) - 1 do printf, unit, lines[i]
  close, unit
  free_lun, unit
end
