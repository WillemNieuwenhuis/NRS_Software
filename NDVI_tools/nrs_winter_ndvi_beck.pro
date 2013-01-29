;+
; :Description:
;    Calculate winter NDVI and replace winter months NDVI values with winter NDVI. The
;    winter NDVI is calculated on a per year basis.
;
; :Params:
;    image_name : in, required
;      Name of the input NDVI image
;    outname : out, required
;      Name of the output image
;
; :Keywords:
;    images_per_year : in, optional, default = 36
;      The number of images per year; used to determine which month to use for the calculation
;    first_band_index : in, optional, default = 0
;      Indicates the band number in the image of the start of the first full year (zero based)
;    aggr_method : in, optional, default = max
;      The aggregation type to combine the yearly values (max, min or avg) 
;    prog_obj : in, optional
;      Progress indicator object of type ProgressBar; if empty or NULL no progress is displayed
;    cancelled : out
;      Indicate if the operation was aborted (== 1) or not (== 0)
;
; :Author: nieuwenhuis
;-
pro nrs_winter_ndvi_beck, image_name, outname $
                        , images_per_year = images_per_year $
                        , first_band_index = first_band_index $
                        , aggr_method = aggr_method $
                        , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image_name, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  if n_elements(outname) eq 0 then $
    outname = getoutname(image_name, postfix = '_wnt', ext = '.')
  
  am = 0  ; max
  ams = ['max', 'min', 'mean', 'median']
  if n_elements(aggr_method) gt 0 then begin
    ix = where(ams eq strlowcase(aggr_method), cnt_am)
    if cnt_am eq 1 then am = ix[0]
  endif
  
  cancelled = 0
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then void = temporary(mi)

  if n_elements(first_band_index) eq 0 then first_band_index = 0
  
  nr_years = nb / images_per_year
  if first_band_index ne 0 then nr_years++
  
  if n_elements(images_per_year) eq 0 then images_per_year = 36
  if images_per_year eq 23 then begin
    wix = [indgen(4), indgen(5) + 18] ; define winter period
    winp = indgen(4) + 16             ; input bands for winter NDVI
  endif else if images_per_year eq 36 then begin
    wix = [indgen(6), indgen(5) + 31] ; define winter period
    winp = indgen(4) + 17             ; input bands for winter NDVI
  endif
  winp = (winp + images_per_year - first_band_index + 1) mod images_per_year
  wix = (wix + images_per_year - first_band_index + 1) mod images_per_year
  wix = wix[sort(wix)]

  nrs_set_progress_property, prog_obj, /start, title = 'Calculating Winter NDVI'
  
  openw, unit, outname, /get_lun
  
  slice = make_array(ns, images_per_year * nr_years, type = dt)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice[*, 0 : nb - 1] = envi_get_slice(fid = fid, /bil, line = line, xs = 0, xe = ns - 1)
    slice = reform(slice, ns, images_per_year, nr_years, /overwrite)
    
    valid = slice[*, winp, *]
    ix = where(valid gt 0.0, cnt)
    if (cnt gt 0) and (cnt lt n_elements(valid)) then begin
      case am of
        1 : minmax = min(valid, dim = 3) > 0
        2 : minmax = mean(valid, dim = 3) > 0
        3 : minmax = median(valid, dim = 3) > 0
        else : minmax = max(valid, dim = 3) > 0
      endcase
      
      ndx19 = 2
      ndx20 = 3
      miss19 = where(valid[*, 2, *] le 0.0, cnt19)
      miss20 = where(valid[*, 3, *] le 0.0, cnt20)
      if cnt20 eq nr_years then ndx20 = 1
      if cnt19 eq nr_years then begin
        if cnt20 eq nr_years then ndx19 = 0 $
        else ndx19 = 1
      endif
      
      winter_ndvi = sqrt(minmax[*, ndx19, *] * minmax[*, ndx20, *])
      winter_ndvi = rebin(winter_ndvi, ns, images_per_year, nr_years)
      
      ; apply winter ndvi
      slice[*] >= temporary(winter_ndvi)
    endif else slice >= 0
    
    slice = reform(slice, ns, nr_years * images_per_year, /overwrite)
    writeu, unit, slice[*, 0 : nb - 1]  ; ignore the fill
  endfor

  close, unit
  free_lun, unit
  
  envi_setup_head, fname = outname, data_type = dt, /write $
          , interleave = 1 $  ; BIL
          , nb = nb, nl = nl, ns = ns $
          , descrip = description $
          , map_info = mi $
          , bnames = bnames $
          , data_ignore_value = undef $
          , xstart = xs, ystart = ys
end

pro beck_test
  inp = 'E:\NRS\Mitra\Timesat testdata\klein'
  outp = 'E:\NRS\Mitra\Timesat testdata\klein_out2'
  nrs_winter_ndvi_beck, inp, outp, images_per_year = 23, first = 17
end

