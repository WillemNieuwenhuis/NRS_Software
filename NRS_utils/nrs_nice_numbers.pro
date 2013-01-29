function nrs_nice_number, num, round = round
  compile_opt idl2, logical_predicate
  
  rounding = keyword_set(round)
  
  exp = floor(alog10(num))
  frac = num / (10.0 ^ exp)

  nf = 10.0
  if rounding then begin
    if frac lt 1.5 then nf = 1.0 $
    else if frac lt 3.0 then nf = 2.0 $
    else if frac lt 7.0 then nf = 5.0
  endif else begin
    if frac le 1.0 then nf = 1.0 $
    else if frac le 2.0 then nf = 2.0 $
    else if frac le 5.0 then nf = 5.0
  endelse
   
  return, nf * (10 ^ exp)
end

;+
; :Description:
;    For the range (low, high) calculate <i>nice numbers</i> for graph axis labelling.
;    The routine is based on the nice number algorithm of Paul S. Hecbert (Graphics Gems, p61;
;    Google Books link: http://books.google.nl/books?id=fvA7zLEFWZgC&lpg=PA61&hl=nl&pg=PA61#v=onepage&q&f=false   
;
; :Params:
;    range : in, required
;      An array with at least two numbers; the min and max values are used for the algorithm
;
; :Keywords:
;    num_ticks : in, default = 6
;      The desired numbers of tickmark labels. Note this is only used as a hint; the actual number
;      of labels will lie close to it, but is not necessarily the same
;    loose : in, default = false
;      Indicate whether the labels should be <b>tight</b>, that is in the range of the input, or it can be
;      <b>loose</b>, that is the range is extended to include the first nice numbers just outside the range.
;
; :Author: nieuwenhuis
; 
; :History:
;   2012 oct 24
;-
function nrs_nice_numbers, range, num_ticks = num_ticks, loose = loose
  compile_opt idl2, logical_predicate
  
  tight = ~keyword_set(loose)
  
  if n_elements(num_ticks) eq 0 then num_ticks = 6 $
  else num_ticks = fix(num_ticks)
  
  mx = max(range, min = mn)
  if mn eq mx then return, []
  
  mm = nrs_nice_number(mx - mn, round = 0)
  tms = nrs_nice_number(mm / (num_ticks - 1), /round)
  gn = floor(mn / tms) * tms
  gx = ceil(mx / tms) * tms
  frac = max([-floor(alog10(tms)), 0])
  
  nn = []
  for g = gn, gx + 0.5 * tms, tms do begin
    nn = [nn, g]
  endfor
  
  if tight then begin
    if (nn[1] - range[0]) lt 0.5 * tms then nn = nn[1 : -1]
    nn[0] = range[0]
    if (range[1] - nn[-2]) lt 0.5 * tms then nn = nn[0 : -2]
    nn[-1] = range[1]
  endif
  
  if frac eq 0 then nn = long(nn) 
  return, nn
end
