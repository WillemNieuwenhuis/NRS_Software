;+
; :Description:
;    Read multiple bands from an image.
;
; :Params:
;    imageid : in, required
;      The ENVI handle of the image
;    bands : in, required
;      An array with the band numbers to read
;
; :Keywords:
;    silent : in, optional, default = 0
;      Enables console messages when set
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function readBands, imageid, bands, silent = silent
  compile_opt idl2
   
  if ~keyword_set(silent) then print, '   + reading bands'
  
   info = getimageinfo(imageid)
   nbbands = n_elements(bands)
   banddata = fltarr(info.nbrows, info.nbcolumns, nbbands)
   
   for b = 0, nbbands - 1 do begin
     banddata[*, *, b] = envi_get_data(fid = imageid, pos = b, dims = info.dimensions)
   endfor
   
   return, banddata
end