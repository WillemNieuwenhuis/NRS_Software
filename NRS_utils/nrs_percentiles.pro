;+
; :description:
;    Calculate percentiles.
;
; :returns:
;   For each percentile in the input the percentile value in the input array is returned.
;
; :params:
;    data : in
;      The data rom which to calculate the percentiles
;
; :keywords:
;    percentiles : in, optional, default = [0.5]
;      A single value of an one-dimensional array with percentiles. The values can either be
;      in the range of 0 to 1.0 or 0 to 100 (%).
;
; :author: nieuwenhuis
; :history:
;   Changes::
;     sept 2013 : created
;-
function nrs_percentiles, data, percentiles = percentiles
  compile_opt idl2, logical_predicate
  
  if n_elements(percentiles) eq 0 then percentiles = [0.5]
  mx = max(percentiles)
  if (mx gt 1.0) && (mx le 100.0) then percentiles /= 100.0 
  
  ix = sort(data)
  
  index = n_elements(data) * percentiles
  
  return, data[ix[index]]
end
