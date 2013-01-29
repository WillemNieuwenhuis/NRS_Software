;+
; :Description:
;    Calculate the RPD (Relative Phenological Development on a per pixel basis.
;
; :Params:
;    image : in, required
;      Name of the input NDVI image
;
; :Keywords:
;    outname : out, optional
;      Name of the output image
;    yearly : in, optional, default = No
;      Indicate that the RPD should be calculated for each year separately
;    images_per_year : in, optional, default = 36
;      The number of images per year; used to determine which month to use for the calculation
;    first_band_index : in, optional, default = 0
;      Indicates the band number in the image of the start of the first full year (zero based)
;    prog_obj : in, optional
;      Progress indicator object of type ProgressBar; if empty or NULL no progress is displayed
;    cancelled : out
;      Indicate if the operation was aborted (== 1) or not (== 0)
;
; :Author: nieuwenhuis
;-
pro nrs_rpd, image, outname = outname $
                  , yearly = yearly $
                  , images_per_year = images_per_year $ 
                  , first_band_index = first_band_index $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , fname = image_name $
                 , bnames = bnames $
                 , data_ignore_value = nodata
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  cancelled = 0

  if strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(image_name, ext = '.', postfix = '_rpd')

  ; open the output for writing
  openw, unit, outname, /get_lun

  nrs_set_progress_property, prog_obj, /start, title = 'Calculating RPD'
  
  pos = indgen(nb)
  idx = rebin(indgen(ns), ns, nb)
  
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    ndvi = envi_get_slice(fid = fid, /bil $
                            , pos = pos $
                            , line = l, xs = 0, xe = ns - 1 $
                          )
    ndvi_min = min(ndvi, dim = 2, max = ndvi_max)
    range = (ndvi_max - ndvi_min)
    rpd = (ndvi - ndvi_min[idx]) / range[idx]
    writeu, unit, rpd
  endfor 

  close, unit
  free_lun, unit  ; close assoc
  
  envi_setup_head, fname = outname $
        , data_type = 4 $   ; float
        , /write $
        , xstart = xs, ystart = ys $
        , interleave = 1 $  ; BIL
        , nb = nb, nl = nl, ns = ns $
        , bnames = bnames $
        , map_info = mi $
        , data_ignore_value = nodata
end
