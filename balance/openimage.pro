;+
; :Description:
;    Open an image.
;
; :Params:
;    filename : in, required
;      The name of the image file
;    imageid : out
;      The ENVI handle of the opened image
;
; :Keywords:
;    silent : in, optional, default = 0
;      Enables console messages when set
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
pro openImage, filename, imageid, silent = silent
  compile_opt idl2

  if ~keyword_set(silent) then print, '   + opening image'
  
;  loadct, 0, /silent
  envi_open_file, filename, r_fid = imageid, /no_realize, /no_interactive_query
  if imageid eq -1 then print, 'error on opening ', filename
end