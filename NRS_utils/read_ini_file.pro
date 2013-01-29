;+
; :Description:
;   Read key value pairs from a text file. Keys and values are separated by
;   an equal sign (=). Both the key and value are read
;   as strings. The file may contain comments, which are recognized by 
;   starting with a semicolon. An example::
; 
;     ;########################################################################################
;     ; Input parameters for IDL Object Genetic Algorithm
;     ;
;     ; This parameter definition file is read in 'runga.pro'.
;     ;
;     ;########################################################################################
;
;     maxgen=50               ;Maximum number of generations to be carried out
;     nselect=10              ;Nr of kandidates for tournament selection (< pos_size/2)
;     pop_size=100            ;The size of the population
;     p_creep=0.05            ;Probability of creep mutation
;   
; :Params:
;   filename
;     The name of the text file containing the key value pairs
;-
function nrs_ini_read_file, filename
  if (file_info(filename)).exists then begin
     lbl = strarr(file_lines(filename))
     openr, 1, filename
     readf, 1, lbl
     close, 1
     p = strpos(lbl, '=')
     s = strpos(lbl, ';') ; skip everything after comments
     l = strlen(lbl)
     s = abs(min([[s],[l * s]], dim = 2)) ; use entire string if no comment found
     k = strtrim(strmid(lbl, 0, transpose(p)), 2)
     v = strtrim(strmid(lbl, transpose(p + 1), transpose(s) - transpose(p + 1)), 2)
     ix = where(k ne '')
     return, {key:k[ix], val:v[ix]}
  endif else return,0
end

function nrs_ini_get_value, kv, key
  ix = where(kv.key eq key, count)
  if count eq 0 then return, ''

  return, strsplit(kv.val[ix], '''"',/extract)
end

function nrs_ini_get_array, kv, key
  ix = where(kv.key eq key, count)
  if count eq 0 then return, ''
  
  return, strsplit(kv.val[ix], ' ,' + string(9b) ,/extract)
end

