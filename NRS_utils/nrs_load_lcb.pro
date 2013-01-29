;+
; :Description:
;   From an open image load either a single line or a single column from a single band.
;   <b>Line<b> is default if none of the keywords <b>line</b>, <b>col</b> is specified.
; 
; :Params:
;   fid : in
;     The ID of the open image
;
; :Keywords:
;   line : in, default = 1
;     The line to retrieve the data for; takes preference over the col keyword
;     The line value is clipped to the range in the image
;   col : in
;     The column to retrieve the data for
;     The col value is clipped to the range in the image
;   band : in, default = 0
;     The band number to get the data for; if not specified, the first band is selected
;   
; :Returns:
;   Single line or single column data. 
;-
function nrs_load_lcb, fid, line = line, col = col, band = band
  compile_opt idl2, logical_predicate
  
  doLine = n_elements(line) eq 1
  doCol = ~doLine && n_elements(col) eq 1
  doLine = ~doLine && ~doCol
  if n_elements(band) eq 0 then band = 0
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb
  
  if doLine then dims[3:4] = (line > 0) < nl
  if doCol then dims[1:2] = (col > 0) < ns

  band = (band > 0) < nb
  
  return, envi_get_data(fid = fid, pos = band, dims = dims)
end
