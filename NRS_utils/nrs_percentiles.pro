function nrs_percentiles, data, percentiles = percentiles
  compile_opt idl2, logical_predicate
  
  if n_elements(percentiles) eq 0 then percentiles = [0.5]
  mx = max(percentiles)
  if (mx gt 1.0) && (mx le 100.0) then percentiles /= 100.0 
  
  ix = sort(data)
  
  index = n_elements(data) * percentiles
  
  return, data[ix[index]]
end
