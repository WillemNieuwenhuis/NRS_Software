;+
; :Description:
;    Calculate the length of the longest sequence of non-zero values (zeroes == 0)
;    or zeroes (zeroes == 1)
;
; :Params:
;    rij:
;      The input value array (only zeroes and one other value) 
;    seqmax:
;      Ouput = the length of the longest sequence of the single value
;
; :Keywords:
;   zeroes:
;   
; :Example:
;   rij = [0, 0, 25, 25, 25, 25, 0, 25, 25, 25, 0, 0, 0]<br>
;   Running: <b>nrs_calc_seq, rij, segmax</b>
;     gives segmax = 4<br>
;   Running: <b>nrs_calc_seq, rij, segmax, /zeroes</b>
;     gives segmax = 3
;   
; :Author: Willem Nieuwenhuis
;-
pro nrs_calc_seq, rij, seqmax, zeroes = zeroes
  ones = 1 - (rij gt 0)
  if n_elements(zeroes) gt 0 then $
    ones = 1 - ones 
  cc = fix(total(ones, /cum) - 1)
  h = histogram(cc, /binsize)
  seqmax = max(h) - 1
end
