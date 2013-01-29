;+
; :Description:
;    Close an open image.
;
; :Params:
;    imageid : in, required
;      The ENVI handle of the image to close
;
; :Keywords:
;    silent : in, optional, default = 0
;      Enables console messages when set
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
pro closeImage, imageid, silent = silent
  compile_opt idl2

  if ~keyword_set(silent) then print, '   + closing image'
  envi_file_mng, id = imageid, /remove
end