;+
; :Description:
;    Write a line of text to a file.
;
; :Params:
;    logfile : in, required
;      Name of the file to write the text to
;    line : in, required
;      The line of text to write
;
; :Keywords:
;    append : in, optional
;      If set append the line to the end of the file; otherwise
;      overwrite the content with the new line of text
;    header : in, optional
;      If set only write the line if the file is empty
;
; :Author: nieuwenhuis
;-
pro nrs_log_line, logfile, line, append = append, header = header, use_unit = unit
  compile_opt idl2

  if keyword_set(header) then begin
    if (file_info(logfile)).exists then $
      if file_lines(logfile) gt 0 then return
  endif

  if (n_elements(unit) gt 0) && (unit gt 0 && unit lt 100) then $
    openw, unit, logfile, append = append $
  else $
    openw, unit, logfile, /get_lun, append = append
  printf, unit, line
  close, unit
  free_lun, unit
end

function nrs_auto_clust_status, logfile
  compile_opt idl2
  
  openr, unit, logfile, /get_lun
  line = ''
  while ~eof(unit) do begin
     readf, unit, line
  endwhile
  close, unit
  free_lun, unit

  line = strtrim(line, 2)
  if stregex(line, '[0-9]+') ne 0 then return, -1
  
  clus = fix(line)
  return,clus
end

