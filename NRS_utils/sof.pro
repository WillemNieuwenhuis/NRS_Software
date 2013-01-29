;+
; Show all open ENVI files
;-
pro sof, bandnames = bandnames
  fids = envi_get_file_ids()  
  if (fids[0] eq -1) then return  
  for i = 0, n_elements(fids) - 1 do begin  
     envi_file_query, fids[i], fname = fname, bnames = bnames
     if n_elements(bandnames) eq 1 then $  
       print, fids[i], ' = ', file_basename(fname), ', ', bnames $
     else $
       print, fids[i], ' = ', file_basename(fname)
  endfor
end

;+
; Get a list of names open files (full path)
;-
function lof
  fids = envi_get_file_ids()  
  if (fids[0] eq -1) then return, -1
  ar = strarr(n_elements(fids))
  for i = 0, n_elements(fids) - 1 do begin  
     envi_file_query, fids[i], fname = fname  
     ar[i] = fname  
  endfor
  return, ar
end