;+
; :Description:
;    Calculate the magnitude. Magnitude is defined as the difference
;    between the pixel value and the sum of the average and standard deviation.
;    The average and SD are calculated for image locations filtered by class
;    In this calculation the SD optionally already has been multiplied by an user-defined
;    factor (1 to 3.5, in increments of 0.5)
;
; :Params:
;    ldata : in
;      Layer image data
;    cldata : in
;      Classified image
;    segdata : in
;      All connected areas
;    poolsd : in
;      Lookup table with the historical pooled SD values
;    ndvi_py : in
;      The number of layers per year
;    lpy : in
;      The current layer index
;
; :Keywords:
;    magdata : in, out
;      Output magnitude values
;    as_perc : in, default = false
;      Calculate the magnitude as percentage, so (val - (avg + sd)) / (val - avg)
;      If true the setting of abs_diff is ignored
;    abs_diff: in, default = true
;      If set calculate only positive differences; is not set calculate the actual differences
;
; :Author: nieuwenhuis
;-
pro nrs_ndvi_magdata, ldata, cldata, segdata, poolsd, ndvi_py, lpy, magdata = magdata $
                    , as_perc = as_perc $
                    , abs_diff = abs_diff
  compile_opt idl2, logical_predicate
  
  abs_diff = keyword_set(abs_diff)
  as_perc = keyword_set(as_perc)
  magdata[*] = 0.0
  diff = magdata - magdata   ; all zeroes
  clh = histogram(segdata, binsize = 1, omin = cmin, omax = cmax, reverse_indices = ri)
  for j = 0L, n_elements(clh) - 1 do begin
    if ri[j + 1] gt ri[j] then begin
      clx = ri[ri[j] : ri[j + 1] - 1]
      avg = total(ldata[clx]) / (ri[j+1] - ri[j])

      cl = cldata[clx[0]]
      sd = poolsd[cl * ndvi_py + lpy]

      diff[clx] = abs(ldata[clx] - avg)   ; differences from avg in class
      if as_perc then begin
        magdata[clx] = ((diff[clx] - sd) > 0) / diff[clx]
      endif else begin
        sign = abs_diff ? 1 : (ldata[clx] gt avg) * 2 - 1
        magdata[clx] = sign * ((diff[clx] - sd) > 0) ; only use magnitude outside the avg+/-sd*mult
       endelse
    endif
  endfor      
end

;+
; :Description:
;    Using the classes in the classified image as masks, calculate
;    the difference of the layer.
;    <p>
;    Procedure for all classes:
;    <ol>
;    <li>select all locations for the class
;    <li>calculate the average value of these locations in the data layer
;    <li>calculate the difference of each of these locations with the average
;    </ol>
;
; :Params:
;    ldata : in
;      Layer data
;    cldata : in
;      Classified data
;
; :Keywords:
;    diffout : in, out
;      Image that gets filled in with the difference from the average values
;      in the layer as masked by the classes 
;
; :Author: nieuwenhuis
;-
pro nrs_ndvi_diff_from_avg, ldata, cldata, diffout = tl
  tl[*] = 0.0
  clh = histogram(cldata, binsize = 1, omin = cmin, omax = cmax, reverse_indices = ri)
  for j = 0L, n_elements(clh) - 1 do begin
    if ri[j + 1] gt ri[j] then begin
      avg = total(ldata[ri[ri[j] : ri[j + 1] - 1]]) / (ri[j+1] - ri[j])
      tl[ri[ri[j] : ri[j + 1] - 1]] = abs(ldata[ri[ri[j] : ri[j + 1] - 1]] - avg)   ; differences from avg in class
    endif
  endfor      
end

pro nrs_ndvi_sd_lookup, diffin, cldata, nr_class, sd_tab, ndvi_py, lpy $
                      , outdata = outdata, magdata = magdata
  outdata[*] = 0
  magdata[*] = 0.0
  
  for cl = 0, nr_class do begin
    clx = where(cldata eq cl, count)  ; filter on class
    if count eq 0 then continue

    sd = sd_tab[cl * ndvi_py + lpy]
    
    diff = diffin[clx]
    outdata[clx] = (diff gt sd) ; indicate changes
    if total(diff gt sd) gt 0 then begin
      magdata[clx] = (diff - sd) > 0
    endif
  endfor
end

;+
; :Description:
;  Calculate the absolute difference from the average value in each
;  layer of a NDVI stack. The average is calculated for a selection taken from
;  the classified image ref_img. If the ref_img2 classified image is defined, next the
;  difference value is filtered through this second classified image.
;  Then this difference value is compared with the standard
;  deviation that is determined by the lookup table sdlut.
;  The lookup table has sections ordered by class; the class for a
;  location is determined by the refimg.
; 
; :Params:
;   refimg
;     The FID for the reference class image
;   ndvi_stack
;     The image stack containing the NDVI images for several years.
;     The number of layers per year is determined by the ndvi_py keyword.
;   sdlut
;     The lookup table containing the valid ranges for the values in the
;     ndvi stack. It is organised with two columns (class, sd); row-wise
;     it is ordered per class: for each class the number of layers is given.
;   outdata
;     The stacked output with all detected changes; if the calculation is
;     stopped by the user this will be -1
;  
; :Keywords:
;   ndvi_py
;     Number of NDVI layers per year. If missing defaults to 36.
;   bnames
;     List of band names for the output
;   ref_img2
;     Use a second classified image (with courser classes) to do the final selection/lookup
;   mag_data : out
;     For all anomalies contains the magnitude of the outliers
; 
; :Author:
;   Willem Nieuwenhuis
; 
; :History:
;   august 2010: Created
;-
pro nrs_ndvi_change_detection, refimg, ndvi_stack, sdlut, outdata, ndvi_py = ndvi_py, bnames = bnames, ref_img2 = ref_img2, mag_data = mag_data
  if n_elements(ndvi_py) eq 0 then ndvi_py = 36
  
  envi_file_query, refimg, dims = dims, ns = ns, nl = nl, class_names = cnames, num_classes = nrclass
  mi_ref = envi_get_map_info(fid = refimg, undefined = undef_csy)
  
  envi_file_query, ndvi_stack, dims = dimsndvi, ns = nsndvi, nl = nlndvi, nb = nrlayers
  if (ns ne nsndvi) or (nl ne nlndvi) then begin
    outdata = -1
    return
  endif
  
  ; Load classified image
  nrs_load_class_image, refimg, cldata = cldata, cnames = cnames, num_classes = nrclass
  
  if n_elements(ref_img2) gt 0 then begin
    nrs_load_class_image, ref_img2, cldata = cldata2, cnames = cnames2, num_classes = nrclass2
  endif
  
; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Detect NDVI changes")
  progressBar -> Start

  outdata = bytarr(ns * nl, nrlayers)
  mag_data = fltarr(ns * nl, nrlayers)
  lut = read_csv(sdlut, header = header)
  if n_elements(ref_img2) gt 0 then begin
    tl = fltarr(ns * nl)
    for layer = 0, nrlayers - 1 do begin
      progressBar -> Update, 100.0 * layer / (nrlayers - 1), text = 'Progress: ' + string(layer, format = '(i0)') + ' of ' + string(nrlayers, format = '(i0)')
      cancelled = progressBar -> CheckCancel()
      if cancelled eq 1 then begin
        progressBar -> Destroy
        ans = dialog_message('Calculation interrupted by user', title = 'Information', /information)
        outdata = -1
        return
      endif
      ldata = envi_get_data(fid = ndvi_stack, dims = dims, pos = layer)
      lpy = layer mod ndvi_py
      
      ; first differences, ...
      ; select locations by class through histogram
      clh = histogram(cldata, binsize = 1, omin = cmin, omax = cmax, reverse_indices = ri)
      for j = 0L, n_elements(clh) - 1 do begin
        if ri[j + 1] gt ri[j] then begin
          avg = total(ldata[ri[ri[j] : ri[j + 1] - 1]]) / (ri[j+1] - ri[j])
          tl[ri[ri[j] : ri[j + 1] - 1]] = abs(ldata[ri[ri[j] : ri[j + 1] - 1]] - avg)  ; differences from avg in class
        endif
      endfor      
      
      ; ... then lookup
      for cl = 0, nrclass2 - 1 do begin
        clx = where(cldata2 eq cl, count)  ; filter on course grained classes
        if count eq 0 then continue

        ix = cl * ndvi_py + lpy;
        sd = lut.(1)[ix] ; sd
        
        diff = tl[clx]
        outdata[clx, layer] = (diff gt sd) ; indicate changes
        if total(diff gt sd) gt 0 then begin
          mag_data[clx, layer] = (diff - sd) > 0
        endif
      endfor
    endfor
  endif else begin
    for layer = 0, nrlayers - 1 do begin
      progressBar -> Update, 100.0 * layer / (nrlayers - 1), text = 'Progress: ' + string(layer, format = '(i0)') + ' of ' + string(nrlayers, format = '(i0)')
      cancelled = progressBar -> CheckCancel()
      if cancelled eq 1 then begin
        progressBar -> Destroy
        ans = dialog_message('Calculation interrupted by user', title = 'Information', /information)
        outdata = -1
        return
      endif
      ldata = envi_get_data(fid = ndvi_stack, dims = dims, pos = layer)
      lpy = layer mod ndvi_py
      for cl = 0, nrclass - 1 do begin
        clx = where(cldata eq cl, count)  ; filter on class
        if count eq 0 then continue
  
        avg = total(ldata[clx]) / count
        
        ix = cl * ndvi_py + lpy;
        sd = lut.(1)[ix] ; sd
        
        diff = abs(ldata[clx] - avg) ; indicate changes
        outdata[clx, layer] = diff gt sd
        if total(diff gt sd) gt 0 then begin
          mag_data[clx, layer] = (diff - sd) > 0
        endif
      endfor
    endfor

  endelse
  ; build band names
  nn = indgen(nrlayers)
  bnames = 'Year.nr ' + string(1 + nn / ndvi_py, format = '(I0)') + '.' + string(1 + nn mod ndvi_py, format = '(I0)')
  
  progressBar -> Destroy
  
  outdata = reform(outdata, ns, nl, nrlayers, /overwrite)
  mag_data = reform(mag_data, ns, nl, nrlayers, /overwrite)
end

pro nrs_ndvi_change_detection_cmd, ndvi_stack, class_map, lut
  envi_open_file, ndvi_stack, r_fid = ndvi
  if ndvi eq -1 then return
  
  envi_open_file, class_map, r_fid = class
  if class eq -1 then return
  
  envi_file_query, class, dims = dims
  mi_ref = envi_get_map_info(fid = class, undefined = undef_csy)
  
  nrs_ndvi_change_detection, class, ndvi, lut, outdata, bnames = bnames
  
  outname = getoutname(ndvi_stack, postfix = '_change', ext = '.')  ; remove extension
  
  if undef_csy eq 1 then $
    envi_write_envi_file, outdata, out_name = outname, bnames = bnames $
  else $
    envi_write_envi_file, outdata, out_name = outname, bnames = bnames, map_info = mi_ref
    
end