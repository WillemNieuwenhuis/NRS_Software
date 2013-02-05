;+
; :Description:
;    Create a mask file from a quality band by extracting only the
;    quality bits in the mask_val.
;
; :Params:
;    filename : in, required
;      The image band with the quality information 
;
; :Keywords:
;    mask_val : the value representing the bits to extract
;
; :Author: nieuwenhuis
; :History:
;   nov 2012 : Initial implementation
;-
pro nrs_build_cloud_mask, filename, mask_val = mask_val
  compile_opt idl2
  
  envi_open_file, filename, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, dims = dims
  data = envi_get_data(fid = fid, dims = dims, pos = 0)
  mi = envi_get_map_info(fid = fid, undef = undef)
  if undef eq 1 then void = temporary(mi)
  
  if n_elements(mask_val) eq 0 then mask_val = 3
  
  mask = (data and mask_val) ne mask_val
  
  outname = getoutname(filename, postfix = '_mask', ext = '.')
  envi_write_envi_file, mask, out_name = outname, bnames = ['Mask Band'], map_info = mi
end