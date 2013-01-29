pro nrs_classify_R2, image, band = band $
                   , out_name = outname, prog_obj = progressBar, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  cancelled = 0
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  if nb gt 1 then begin
    if n_elements(band) eq 0 then begin
      print, 'Warning: Taking band 1 as R2'
      band = 0
    endif
  endif
  
  has_undef = n_elements(undef) gt 0
  if n_elements(undef) eq 0 then undef = -99.0
  if undef eq 1.0e-34 then undef = -99.0
  
  if n_elements(outname) eq 0 || strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(image, ext = '.', postfix = '_class')

;  nrs_set_progress_property, prog_obj, title = 'Classifying R2', /start
  cancelled = 0
  
  cla_data = bytarr(ns * nl)
;    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
  data = envi_get_data(fid = fid, dims = dims, pos = band)
  ix = where(data le 1.0, count)
  if count gt 0 then cla_data[ix] = 5 ; Excellent fit
  ix = where(data lt  0.8, count)
  if count gt 0 then cla_data[ix] = 4 ; Good fit
  ix = where(data lt  0.6, count)
  if count gt 0 then cla_data[ix] = 3 ; Poor fit
  ix = where(data lt  0.4, count)
  if count gt 0 then cla_data[ix] = 2 ; Very poor fit
  ix = where(data lt 0.2, count)
  if count gt 0 then cla_data[ix] = 1 ; No fit
  if has_undef then begin
    ix = where(data eq undef, count)
    if count gt 0 then cla_data[ix] = 0 ; unclassified
  endif

  classnames = ['Unclassified', 'No fit', 'Very poor fit', 'Poor fit' $
              , 'Good fit', 'Excellent fit']
  lookup = [ [0, 0, 0] $  ; Unclassified
           , [255, 0, 0] $
           , [255, 255, 0] $
           , [0, 255, 0] $
           , [0, 255, 255] $
           , [0, 0, 255] $
           ]
  cla_data = reform(cla_data, ns, nl, /overwrite)
  envi_write_envi_file, cla_data, out_name = outname $
            , bnames = 'Fit' $
            , map_info = mi $
            , file_type = envi_file_type('envi classification') $
            , class_names = classnames $
            , num_classes = n_elements(classnames) $
            , lookup = lookup $
            , descrip = 'Classification of coefficient of determination' $
            , xstart = xs, ystart = ys $
            , data_ignore_value = 0
            
end
