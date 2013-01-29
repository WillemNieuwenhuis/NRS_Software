pro nrs_log_line, logfile, line, append = append
  openw, unit, logfile, /get_lun, append = append
  printf, unit, line
  close, unit
end

function nrs_auto_clust_status, logfile
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

