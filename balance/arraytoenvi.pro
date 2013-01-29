;+
; :Description:
;    Create a new ENVI file im memory copying all metadata
;    from a source image and new image data
;
; :Returns:
;    The ENVI image ID for the new image
;
; :Params:
;    imagedata : in, required
;      Array (2D or 3D) with the data for the new memory image file
;    copyimageid : in, required
;      The ENVI image handle of the source image
;
; :Keywords:
;    silent : in, optional, default = 0
;      Enables console messages when set
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function arrayToENVI, imagedata, copyimageid, silent = silent
  compile_opt idl2
  
  if ~keyword_set(silent) then print, '   + importing array in envi'
  
  info = getimageinfo(copyimageid)
  
  nbbands = 1
	nbdims = size(imagedata, /n_dimensions)
	if nbdims gt 2 then nbbands = dims[2]

  inheritdata = envi_set_inheritance(copyimageid, info.dimensions, indgen(nbbands), /full)
  envi_enter_data, imagedata, inherit = inheritdata, r_fid = newid
  
  return, newid
end