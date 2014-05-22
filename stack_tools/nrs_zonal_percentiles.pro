;+
; :description:
;    Calculate percentiles per zone on an image stack
;
; :params:
;    image : in
;      The image stack
;    classfile : in
;      The classified image containing the zonal information. The spatial dimensions
;      are expected to be equal to those of the image stack
;
; :keywords:
;    percentile : in, optional, default = 50% (median)
;      The percentile (in percentage) to calculate; this can be a comma-separated list of percentiles 
;    ignore_value : in, optional
;      Indicate the missing value in the data in the image stack
;    prog_obj : in, optional
;      A progressBar object to be used to display progress
;    cancelled : out
;      Indicates if the process was interupted by the user in the progressBar
;
; :Author: nieuwenhuis
; 
; :history:
;   - 22 May 2014: nieuwenhuis, created
;-
pro nrs_zonal_percentiles, image, classfile $
                         , outname = outname $
                         , percentile = percentile $
                         , ignore_value = ignore_value $
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
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_type = dt
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  if undefined eq 1 then delvar, mi
  
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

  nrs_load_class_image, class, cldata = cldata, num_classes = nr_class
  ; calculate masks of all classes
  h = histogram(cldata, min = 0, max = max(cldata), binsize = 1, reverse_indices = ri)
  
  out_table = fltarr(nb, nr_class, n_elements(percentile))
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

    band = envi_get_data(fid = fid, dims = dims, pos = b)
    for c = 0, n_elements(h) - 1 do begin
      
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
    endfor
  endfor
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