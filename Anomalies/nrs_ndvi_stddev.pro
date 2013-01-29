;+
; :Description:
;    Calculate a column vector, containing the standard deviation
;    of the ldata layer filtered per class in the class data
;
; :Params:
;    ldata : in, required
;      2D data to calculate standard deviation over
;    cldata : in, required
;      Class data (2D) for filtering the layer data
;    nrclass : in, required
;      The number of classes in the class image; class number are
;      assumed to be in sequence
;    band : in, required
;      The sequence number of the layer being processed
;    stddev_out : out
;      Matrix with number of layers times number of classes; each cell
;      has the standard deviation for all pixels in the data
;      layer for the particular class; only one column is changed at the time
;
; :Author: nieuwenhuis
;-
pro nrs_stack_stddev, ldata, cldata, nrclass, band, stddev_out
  for cl = 0, nrclass - 1 do begin
    ix = where(cldata eq cl, count)
    if count eq 0 then continue
    
    data = ldata[ix]
    stddev_out[band, cl] = stddev(data)
  endfor
end

;+
; :Description:
;    Calculate standard deviations for a layer stack by
;    class. The classes come from a classified image
;
; :Params:
;    stack : in, required
;      The layer stack FID
;    class : in, required
;      The class image FID
;
; :Keywords:
;    stdevs : out
;      All the standard deviations per class per layer (2D matrix: layer X class)
;    prog_obj : in
;      Optional progress indicator object
;
; :Author: nieuwenhuis
;-
pro nrs_calc_class_stddev, stack, class, stdevs = stats, prog_obj = prog_obj, cancelled = cancelled
  doProgress = n_elements(prog_obj) gt 0
  cancelled = 0
  
  envi_file_query, stack, nb = nb, ml = nl, ns = ns, dims = dims
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass
  stats = fltarr(nb, nrclass)
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
    ldata = envi_get_data(fid = stack, pos = b, dims = dims)
    nrs_stack_stddev, ldata, cldata, nrclass, b, stats
  endfor
end

pro nrs_calc_pooled_sd, stdevs, img_per_year, poolsd = poolsd
  sz = size(stdevs, /dim)
  nrlayers = sz[0]
  nryear = nrlayers / img_per_year
  nrclass = sz[1]
  poolsd = fltarr(img_per_year, nrclass)
  ix = indgen(nryear) * img_per_year
  for cl = 0, nrclass - 1 do begin
    for l = 0, img_per_year - 1 do begin
      poolsd[l, cl] = sqrt(total(stdevs[ix + l, cl] ^ 2) / nryear)
    endfor
  endfor
  poolsd = reform(poolsd, img_per_year * nrclass, /overwrite)
end
