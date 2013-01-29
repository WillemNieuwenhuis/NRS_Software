;+
; :Description:
;    Calculate magnitude as:<p>
;      for each class
;        get std from table
;        get all pixels from layer masked by the class
;        avg = average of these pixels
;        diff = abs(values - avg)
;        magnitude = abs(diff - std)
;
; :Params:
;    ndvi
;      ndvi layer from timeseries (2d array: ns x nl)
;    class
;      class image (2d array: ns x nl)
;    table
;      table (1d array: sd per class / layer)
;    nr_class
;      Number of classes in class image
;    layer
;      Layer number in ndvi stack
;
; :Keywords:
;    mag_data : out
;      Result of the calculation (2d array: ns x nl)
;
; :Author: Willem Nieuwenhuis
;-
pro anomaly_calc_magnitudes, ndvi, class, table, nr_class, ndvi_py, layer, mag_data = mag_data
  lpy = layer mod ndvi_py
  mag_data[*] = 0.0
  for cl = 0, nr_class - 1 do begin
    clx = where(class eq cl, count)
    if count eq 0 then continue

    tix = cl * ndvi_py + lpy
    std = table[tix]
    
    avg = total(ndvi[clx]) / count
    diff = abs(ndvi[clx] - avg)
    mag_data[clx] = (diff - std) > 0
  endfor
end

;+
; :Description:
;    Calculate anomalies:<p>
;      For each class:
;        get the valid range from the table
;        get all pixels not satifying the valid range
;        get all pixels from layer masked by the class
;        calc the intersection
;        the pixels in the intersection are set to the class number
;
; :Params:
;    ndvi
;      ndvi layer (2d)
;    class
;      class image (2d)
;    table
;      table with 3 columns containing the valid range per class / layer (in col 2 and 3)
;    nr_class
;      number of classes in the class image
;    ndvi_py
;      number of ndvi layers per year
;    layer
;      current layer number
;
; :Keywords:
;    outdata : out
;      the anomalous data (2d); 0 (zero) means no anomaly, other values
;      indicate anomalies for the class indicated by the value
;
; :Author: Willem Nieuwenhuis
;-
pro anomaly_calc_from_sdrange, ndvi, cldata, table, nr_class, ndvi_py, layer, outdata = outdata
  lpy = layer mod ndvi_py
  outdata[*] = 0
  for cl = 0, nr_class - 1 do begin
    clx = where(cldata eq cl, count)  ; filter on class
    if count eq 0 then continue

    ix = cl * ndvi_py + lpy;
    sdmin = table.(1)[ix] ; sdmin
    sdmax = table.(2)[ix] ; sdmax
    
    ano = where((ndvi lt sdmin) or (ndvi gt sdmax)) ; anomalies for entire layer
    filterx = setIntersection(ano, clx) ; anomalies only for a current class
    filcnt = n_elements(filterx)
    if (filcnt gt 0) and (filterx[0] ge 0) then begin
      outdata[filterx] = cl ; indicate anomalies
    endif
  endfor
end