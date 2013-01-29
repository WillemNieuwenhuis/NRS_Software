;+
; :Description:
;    Read a single band from the image
;
; :Returns:
;   
; :Params:
;    imageid : in, required
;      The ENVI handle of the image
;    band : in, required
;      The zero-based band number
;
; :Keywords:
;    silent : in, optional, default = 0
;      Enables console messages when set to 1
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function readSingleBand, imageid, band, silent = silent
  compile_opt idl2
  
  if ~keyword_set(silent) then print, '   + reading band ', band + 1
  
  info = getimageinfo(imageid)
  banddata = envi_get_data(imageid, pos = band, dims = info.dimensions)
  
  return, banddata
end
