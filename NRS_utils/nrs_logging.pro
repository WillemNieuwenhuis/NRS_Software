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
pro nrs_log_line, logfile, line, append = append, header = header
  compile_opt idl2

  if keyword_set(header) then begin
    if (file_info(logfile)).exists then $
      if file_lines(logfile) gt 0 then return
  endif
  
  openw, unit, logfile, /get_lun, append = append
  printf, unit, line
  close, unit
end

function nrs_auto_clust_status, logfile
  compile_opt idl2
  
  openr, unit, logfile, /get_lun
  line = ''
  while ~eof(unit) do begin
     readf, unit, line
  endwhile
  close, unit

  line = strtrim(line, 2)
  if stregex(line, '[0-9]+') ne 0 then return, -1
  
  clus = fix(line)
  return,clus
end

