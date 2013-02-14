;+
; in place cleaning of a one dimensional array
;-
pro nrs_clean_savgol, data, width = width, degree = degree, order = order
  n = n_elements(data)  
  if n_elements(width) eq 0 then width = 33
  if n_elements(degree) eq 0 then degree = 4
  if n_elements(order) eq 0 then order = 0
  hwidth = fix(width / 2)
  
  ; Savitzky-Golay with user defined width, order and polynomial degree:  
  savgolFilter = savgol(hwidth, hwidth, order, degree)
  data = convol(data, savgolFilter, /edge_truncate) 
end