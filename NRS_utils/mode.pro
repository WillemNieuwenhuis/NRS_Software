function mode, data, dimension = dim
;  sz = size(data, /dim)
;  ndim = size(data, /n_dim)
;  if n_elements(dim) gt 0 && (dim lt 1 || dim gt ndim) then begin
;    message, 'Dimension cannot exceed the maximum dimension of data'
;  endif
  data = data[sort(data)] 
  ix = where(data ne shift(data, -1), cnt) 
  if cnt eq 0 then begin
    mode = data[0]
  endif else begin 
    mx = max(ix - [-1, ix], pos) 
    mode = data[ix[pos]] 
  endelse
  
  return, mode
end