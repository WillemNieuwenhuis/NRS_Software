pro nrs_classify_spi, spi_image, out_name = outname, prog_obj = progressBar, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, spi_image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  cancelled = 0
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , bnames = bnames $
                 , xstart = xs, ystart = ys $
                 , data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  has_undef = n_elements(undef) gt 0
  if n_elements(undef) eq 0 then undef = -99.0
  if undef eq 1.0e-34 then undef = -99.0
  
  if n_elements(outname) eq 0 || strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(spi_image, ext = '.', postfix = '_class')

  nrs_set_progress_property, prog_obj, title = 'Classifying SPI', /start
  cancelled = 0
  
  cla_data = bytarr(ns * nl, nb)
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
    data = envi_get_data(fid = fid, dims = dims, pos = b)
    ix = where(data ge  2.0, count)
    if count gt 0 then cla_data[ix, b] = 7 ; extremely wet
    ix = where(data lt  2.0, count)
    if count gt 0 then cla_data[ix, b] = 6 ; severely wet
    ix = where(data lt  1.5, count)
    if count gt 0 then cla_data[ix, b] = 5 ; moderately wet
    ix = where(data lt  1.0, count)
    if count gt 0 then cla_data[ix, b] = 4 ; normal
    ix = where(data lt -1.0, count)
    if count gt 0 then cla_data[ix, b] = 3 ; moderately dry
    ix = where(data lt -1.5, count)
    if count gt 0 then cla_data[ix, b] = 2 ; severely dry
    ix = where(data lt -2.0, count)
    if count gt 0 then cla_data[ix, b] = 1 ; extremely dry
    if has_undef then begin
      ix = where(data eq undef, count)
      if count gt 0 then cla_data[ix, b] = 0 ; unclassified
    endif
  endfor

  classnames = ['Unclassified', 'extremely dry', 'severely dry', 'moderately dry' $
              , 'normal', 'moderately wet', 'severely wet', 'extremely wet']
  lookup = [ [0, 0, 0] $  ; Unclassified
           , [255, 0, 0] $
           , [255, 255, 0] $
           , [128, 128, 0] $
           , [0, 255, 0] $
           , [0, 128, 128] $
           , [0, 255, 255] $
           , [0, 0, 255] $
           ]
  cla_data = reform(cla_data, ns, nl, nb, /overwrite)
  envi_write_envi_file, cla_data, out_name = outname $
            , bnames = bnames $
            , map_info = mi $
            , file_type = envi_file_type('envi classification') $
            , class_names = classnames $
            , num_classes = n_elements(classnames) $
            , lookup = lookup $
            , descrip = 'Standardized precipitation index (SPI)' $
            , xstart = xs, ystart = ys $
            , data_ignore_value = 0
            
end
