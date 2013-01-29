;+
; :Description:
;   Read file containing a two or three column table with start and end values for all the parameters
;      in the order: n, cab, cw, cm, scale, lai, ala, hot.
;   <p>
;   If the file contains three columns, the first column is skipped assuming it contains the name of the parameter.
;
; :Params:
;    filename
;      Name of the text file (csv-formatted)
;
; :Keywords:
;    rem_colhdr : in, optional
;      if specified remove the first column
;
; :Author: nieuwenhuis
;-
function nrs_csv_read, filename, rem_colhdr = rem_colhdr
  res = read_ascii(filename, count = nr_rec, delimiter = ',')
  rsf = res.field1
  if n_elements(rsf[*, 0]) gt 1 then begin
    if n_elements(rem_colhdr) gt 0 then $
      data = rsf[1:2,*] else data = rsf
  endif
  
  return, data
end

