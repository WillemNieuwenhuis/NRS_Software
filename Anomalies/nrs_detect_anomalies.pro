;+
; :Description: Calculate the anomalies in the NDVI stack values. An anomaly is
; defined as a stack value not fitting in a range from the lookup
; table sdlut. The lookup table has sections ordered by class; the class for a
; location is determined by the refimg.
; 
; :Params:
;   refimg
;     The FID for the reference class image
;   ndvi_stack
;     The image stack containing the NDVI images for several years.
;     The number of layers per year is determined by the ndvi_py keyword.
;   sdlut
;     The lookup table name containing the valid ranges for the values in the
;     ndvi stack. It is organised with three columns (class, sdmin, sdmax); row-wise
;     it is ordered per class: for each class the number of layers is given.
;   outdata
;     The stacked output with all found anomalies; if the calculation is
;     stopped by the user this will be -1
;  
; :Keywords:
;   ndvi_py
;     Number of NDVI layers per year. If missing defaults to 36.
; 
; :Author:
;   Willem Nieuwenhuis
; 
; :History:
;   august 2010: Created
;-
pro nrs_detect_anomalies, refimg, ndvi_stack, sdlut, outdata, ndvi_py = ndvi_py, bnames = bnames
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
  
  outdata = bytarr(ns * nl, nrlayers)
  outlayer = bytarr(ns * nl)
  lut = read_csv(sdlut, header = header)
  if n_elements(header) lt 3 then begin
    ans = dialog_message('Lookup table should have 3 columns at least, only 2 found', /error)
    return
  endif
  
; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Finding anomalies")
  progressBar -> Start

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
    anomaly_calc_from_sdrange, ldata, cldata, lut, nrclass, ndvi_py, layer, outdata = outlayer    
    outdata[*, layer] = outlayer ; indicate anomalies
  endfor
  
  ; build band names
  nn = indgen(nrlayers)
  bnames = 'Year.nr ' + string(1 + nn / ndvi_py, format = '(I0)') + '.' + string(1 + nn mod ndvi_py, format = '(I0)')
  
  progressBar -> Destroy
  
  outdata = reform(outdata, ns, nl, nrlayers, /overwrite);

end
