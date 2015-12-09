;+
; :description:
;    Calculate percentiles per zone on an image stack. The output is stored in a table.
;    Optionally for each percentile a raster band is created.
;
; :params:
;    image : in
;      The image stack
;    classfile : in
;      The classified image containing the zonal information. The spatial dimensions
;      are expected to be equal to those of the image stack
;
; :keywords:
;    outname : in, optional
;      Output name of the table; base output name of the optional raster bands.
;      If not specified the input data filename will be used as template.
;    percentile : in, optional, default = 50% (median)
;      The percentile (in percentage) to calculate; this can be a comma-separated list of percentiles 
;    ignore_value : in, optional
;      Indicate the missing value in the data in the image stack
;    create_raster : in, optional, default = no
;      If true the software will generate a separate band for each of the percentiles
;    prog_obj : in, optional
;      A progressBar object to be used to display progress
;    cancelled : out
;      Indicates if the process was interupted by the user in the progressBar
;
; :Author: nieuwenhuis
; 
; :history:
;   - 22 May 2014: nieuwenhuis, created
;   - 20 Feb 2015: nieuwenhuis, added creation of raster bands
;
;-
pro nrs_zonal_percentiles, image, classfile $
                         , outname = outname $
                         , percentile = percentile $
                         , ignore_value = ignore_value $
                         , create_raster = create_raster $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  if n_elements(percentile) gt 0 then begin
    percentile = fix(strsplit(percentile, ',', /extract))
    ix = where((percentile gt 0) and (percentile lt 100), cix)
    if cix eq 0 then begin
      void = error_message('Percentiles should be between 0% and 100%', title = 'Zonal percentile', /error, /noname, traceback = 0)
      return
    endif
    percentile = percentile[ix]
  endif $
  else percentile = 50  ; median
  percentile /= 100.0
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  if undefined eq 1 then delvar, mi
  inherit = envi_set_inheritance(fid, dims, /full)
  
  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]

  envi_file_query, class, ns = ns_class, nl = nl_class
  if (ns ne ns_class) || (nl ne nl_class) then begin
    void = error_message('Dimension of class image does not match input image', title = 'Zonal percentile', /error, /noname, traceback = 0)
    return 
  endif
  
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_pctl', ext = '.csv')

  cancelled = 0

  ix = where(dt eq [1, 2, 3, 12, 13, 14, 15], cix)
  isInt = cix gt 0  
  eps = 1.0e-6

  nrs_set_progress_property, prog_obj, /start, title = 'Zonal percentiles'

  nrs_load_class_image, class, cldata = cldata, num_classes = nr_class $
        , has_unclassified = has_unclassified $
        , /class_adjust
  ; calculate masks of all classes
  maxcl = max(cldata)
  h = histogram(cldata, min = 0, max = maxcl, binsize = 1, reverse_indices = ri)

  if keyword_set(create_raster) then begin
    perc_band = make_array(ns * nl, n_elements(percentile), type = dt)
    
    rluns = []
    pnames = []
    foreach p, percentile do begin
      pname = getOutname(outname, postfix = string(p * 100, format = '(i0)'), ext = '.dat')
      openw, lun, pname, /get_lun
      rluns = [rluns, lun]
      pnames = [pnames, pname]
    endforeach
  endif

  out_table = fltarr(nb, nr_class, n_elements(percentile))
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

    if keyword_set(create_raster) then begin
      perc_band[*] = 0
    endif

    band = envi_get_data(fid = fid, dims = dims, pos = b)
    ; for all classes / zones
    for c = 0, maxcl do begin
      
      if ri[c + 1] gt ri[c] then $
        clx = ri[ri[c] : ri[c + 1] - 1]
      selected = band[clx]  ; mask out single class
      if hasIgnore then begin
        ex = where(selected ne ignore_value, cex)
        if cex gt 0 then selected = selected[ex]
      endif
      
      sorted = selected[sort(selected)]
      p_index = percentile * n_elements(sorted)
      
      out_table[b, c, *] = sorted[p_index]
      
      if keyword_set(create_raster) then begin
        for i = 0, n_elements(p_index) - 1 do begin
          perc_band[clx, i] = sorted[p_index[i]]
        endfor
      endif

    endfor
    if keyword_set(create_raster) then begin
      pix = 0
      foreach lun, rluns do begin
        writeu, lun, perc_band[*, pix]
        pix++
      endforeach
    endif

  endfor
  if keyword_set(create_raster) then begin
    pix = 0
    foreach lun, rluns do begin
      close, lun
      free_lun, lun

      envi_setup_head, fname = pnames[pix] $
        , data_type = dt $
        , ns = ns, nl = nl, nb = nb $
        , interleave = 0 $  ; 0 == BSQ
        , bnames = bnames $
        , /write $
        , inherit = inherit

      pix++
    endforeach
  endif

  zeroes = where(abs(out_table) lt eps, czeroes)
  valform = '(f0.6)'
  if isInt then valform = '(i0)'
  out_table = string(out_table, format = valform)
  if czeroes gt 0 then out_table[zeroes] = ''
  out_table = reform(out_table, nb, n_elements(percentile) * nr_class, /overwrite)
  
  header = strjoin(['Percentile,Class', string(indgen(nb) + 1, format = '("Band_", i0)')], ',')
  cli = string((indgen(nr_class * n_elements(percentile)) mod nr_class) + 1, format = '(i0)')
  pin = string(fix(percentile[indgen(nr_class * n_elements(percentile)) / nr_class] * 100), format = '(i0)')
  openw, lun, outname, /GET_LUN
  printf, lun, header
  for l = 0, nr_class * n_elements(percentile) - 1 do begin
    printf, lun, strjoin([pin[l], cli[l], out_table[*, l]], ',')
  endfor
  
  close, lun
  free_lun, lun
  
end

;+
; :description:
;    Calculate percentiles per zone on an image stack. The output is stored in a table.
;    Optionally for each percentile a raster band is created.
;
; :params:
;    image : in
;      The image stack
;    classfile : in
;      The classified image containing the zonal information. The spatial dimensions
;      are expected to be equal to those of the image stack
;
; :keywords:
;    outname : in, optional
;      Output name of the table; base output name of the optional raster bands.
;      If not specified the input data filename will be used as template.
;    percentile : in, optional, default = 50% (median)
;      The percentile (in percentage) to calculate; this can be a comma-separated list of percentiles
;    ignore_value : in, optional
;      Indicate the missing value in the data in the image stack
;    create_raster : in, optional, default = no
;      If true the software will generate a separate band for each of the percentiles
;    prog_obj : in, optional
;      A progressBar object to be used to display progress
;    cancelled : out
;      Indicates if the process was interupted by the user in the progressBar
;
; :author: nieuwenhuis
;
; :history:
;   - 22 May 2014: nieuwenhuis, created
;   - 20 Feb 2015: nieuwenhuis, added creation of raster bands
;   - 7 Dec 2015: nieuwenhuis, split of second routine for grouping
;
;-
pro nrs_zonal_percentiles_group, image, classfile $
  , outname = outname $
  , percentile = percentile $
  , ignore_value = ignore_value $
  , img_per_period = img_per_period $
  , create_raster = create_raster $
  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  if n_elements(percentile) gt 0 then begin
    percentile = fix(strsplit(percentile, ',', /extract))
    ix = where((percentile gt 0) and (percentile lt 100), cix)
    if cix eq 0 then begin
      void = error_message('Percentiles should be between 0% and 100%', title = 'Zonal percentile', /error, /noname, traceback = 0)
      return
    endif
    percentile = percentile[ix]
  endif $
  else percentile = 50  ; median
  percentile /= 100.0

  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return

  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  if undefined eq 1 then delvar, mi
  inherit = envi_set_inheritance(fid, dims, /full)

  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]

  envi_file_query, class, ns = ns_class, nl = nl_class
  if (ns ne ns_class) || (nl ne nl_class) then begin
    void = error_message('Dimension of class image does not match input image', title = 'Zonal percentile', /error, /noname, traceback = 0)
    return
  endif

  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_pctl', ext = '.csv')

  cancelled = 0

  ix = where(dt eq [1, 2, 3, 12, 13, 14, 15], cix)
  isInt = cix gt 0
  eps = 1.0e-6

  nrs_set_progress_property, prog_obj, /start, title = 'Zonal percentiles'

  nrs_load_class_image, class, cldata = cldata, num_classes = nr_class $
    , has_unclassified = has_unclassified $
    , /class_adjust
  ; calculate masks of all classes
  maxcl = max(cldata)
  h = histogram(cldata, min = 0, max = maxcl, binsize = 1, reverse_indices = ri)

  if keyword_set(create_raster) then begin
    perc_band = make_array(ns * nl, n_elements(percentile), type = dt)

    rluns = []
    pnames = []
    foreach p, percentile do begin
      pname = getoutname(outname, postfix = string(p * 100, format = '(i0)'), ext = '.dat')
      openw, lun, pname, /get_lun
      rluns = [rluns, lun]
      pnames = [pnames, pname]
    endforeach
  endif

  nbOut = img_per_period
  nrPeriods = nb / img_per_period
  
  out_table = fltarr(nbOut, nr_class, n_elements(percentile))
  grid = fltarr(ns * nl, nrPeriods, /nozero)
  for period = 0, img_per_period - 1 do begin
    if nrs_update_progress(prog_obj, period, img_per_period, cancelled = cancelled) then return

    if keyword_set(create_raster) then begin
      perc_band[*] = 0
    endif

    ; read the bands to analyse, they are taken from the image stack / time series
    ; at band intervals of img_per_period
    bands = indgen(nrPeriods) * img_per_period + period
    for b = 0, nrPeriods - 1 do begin
      grid[*, b] = envi_get_data(fid = fid, dims = dims, pos = bands[b])
    endfor
    
    ; for all classes / zones
    for c = 0, maxcl do begin
      if ri[c + 1] gt ri[c] then $
        clx = ri[ri[c] : ri[c + 1] - 1]
      selected = grid[clx, *]  ; mask out single class in the collected 3-D cube
      if hasIgnore then begin
        ex = where(selected ne ignore_value, cex)
        if cex gt 0 then selected = selected[ex]
      endif

      sorted = selected[sort(selected)]
      p_index = percentile * n_elements(sorted)

      out_table[period, c, *] = sorted[p_index]

      if keyword_set(create_raster) then begin
        for i = 0, n_elements(p_index) - 1 do begin
          perc_band[clx, i] = sorted[p_index[i]]
        endfor
      endif

    endfor
    if keyword_set(create_raster) then begin
      pix = 0
      foreach lun, rluns do begin
        writeu, lun, perc_band[*, pix]
        pix++
      endforeach
    endif

  endfor
  if keyword_set(create_raster) then begin
    pix = 0
    foreach lun, rluns do begin
      close, lun
      free_lun, lun

      envi_setup_head, fname = pnames[pix] $
        , data_type = dt $
        , ns = ns, nl = nl, nb = nbOut $
        , interleave = 0 $  ; 0 == BSQ
        , bnames = bnames $
        , /write $
        , inherit = inherit

      pix++
    endforeach
  endif

  zeroes = where(abs(out_table) lt eps, czeroes)
  valform = '(f0.6)'
  if isInt then valform = '(i0)'
  out_table = string(out_table, format = valform)
  if czeroes gt 0 then out_table[zeroes] = ''
  out_table = reform(out_table, nbOut, n_elements(percentile) * nr_class, /overwrite)

  header = strjoin(['Percentile,Class', string(indgen(nbOut) + 1, format = '("Band_", i0)')], ',')
  cli = string((indgen(nr_class * n_elements(percentile)) mod nr_class) + 1, format = '(i0)')
  pin = string(fix(percentile[indgen(nr_class * n_elements(percentile)) / nr_class] * 100), format = '(i0)')
  openw, lun, outname, /GET_LUN
  printf, lun, header
  for l = 0, nr_class * n_elements(percentile) - 1 do begin
    printf, lun, strjoin([pin[l], cli[l], out_table[*, l]], ',')
  endfor

  close, lun
  free_lun, lun

end                           