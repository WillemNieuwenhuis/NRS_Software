pro nrs_interpolate_to_payment, image, classfile, table_5, table_25 $
  , img_per_year = img_per_year $
  , outname = outname $
  , start_date = start_date $
  , end_date = end_date $
  , debug = debug $
  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  if n_params() ne 4 then begin
    ans = dialog_message('Not enough parameters to continue', title = 'Error', /error)
    return
  endif
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then  begin
    ans = dialog_message('Could not open timeseries', title = 'Error', /error)
    return
  endif
  
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif
  
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns $
    , data_type = dt, bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = crd_undef)
  if crd_undef eq 1 then mi = temporary(dummy)
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then  begin
    ans = dialog_message('Could not open class map', title = 'Error', /error)
    return
  endif

  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass $
    , has_unclassified = has_unclassified $
    , class_adjust = 0
    
  caldat, sd, mm, dd, yy
  sdy = julday(1,1,yy)
  nrs_get_dt_indices, [sd, ed], julian_out = jo, period = '10-day'
  nrs_get_dt_indices, [sdy, ed], julian_out = joy, period = '10-day'
  offset = where(jo[0] eq joy, cnt)
  
  lut05 = nrs_read_csv(table_5, record_start = 2)
  lut25 = nrs_read_csv(table_25, record_start = 2)
  field_count = n_tags(lut05)
  if n_elements(img_per_year) eq 0 then img_per_year = field_count - 1 ; don't count the start column header
  
  cancelled = 0
  
  classes = lut05.(0) ; // or lut25.(0), they should be identical; the region classes
  perc05 = intarr(n_elements(lut05.(0)), field_count - 1)
  perc25 = intarr(n_elements(lut25.(0)), field_count - 1)
  for c = 1, field_count - 1 do begin
    perc05[*, c - 1] = lut05.(c)
    perc25[*, c - 1] = lut25.(c)
  endfor
  
  nrs_set_progress_property, prog_obj, /start, title = 'Create region indices'
  
  ; create region indices
  nr_classes = n_elements(classes)
  mask = ptrarr(nr_classes)
  count_ar = lonarr(nr_classes)
  for cli = 0, nr_classes - 1 do begin
    cl = classes[cli]
    ix = where(cldata eq cl, cnt)
    mask[cli] = ptr_new(ix)
    count_ar[cli] = cnt
  endfor
  
  nrs_set_progress_property, prog_obj, /start, title = 'Determine pay percentage'
  
  ; process the data one band at a time
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_perc', ext = '.dat')
  outname_d = getoutname(outname, postfix = '_d', ext = '.dat')
  openw, unit, outname, /get_lun
  if debug then openw, unit_d, outname_d, /get_lun
  
  ; first determine year band index
  bind = (indgen(nb) + offset[0]) mod img_per_year
  yind = yy + ((indgen(nb) + offset[0]) / img_per_year)
  out_data = bytarr(ns, nl)
  for band = 0, nb - 1 do begin
    out_data[*] = 0
    if nrs_update_progress(prog_obj, band, nb, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
      file_delete, outname, /quiet, /noexpand_path
      if debug then begin
        close, unit_d
        free_lun, unit_d
        file_delete, outname_d, /quiet, /noexpand_path
      endif
      ptr_free, mask
      return
    endif
    data = envi_get_data(fid = fid, dims = dims, pos = band)
    dx = where(data eq 0, dcnt) ; detect the unclassified values
    
    ; calculate percentage based on the 5% and 25% value bounds
    bix = bind[band]  ; period index
    for cli = 0, nr_classes - 1 do begin
      cur_cl = classes[cli]
      if count_ar[cli] gt 0 then begin
        ix = *mask[cli] ; get the value indices for this class (cur_cl at index cli)
        ; clip all data values witin the 5% and 25% percentile 
        data[ix] = byte((data[ix] > perc05[cli, bix]) < perc25[cli, bix])
        ; apply linear stretch
        out_data[ix] = byte(100.0 * (perc25[cli, bix] - data[ix]) / (perc25[cli, bix] - perc05[cli, bix])) 
      endif
    endfor
    ; keep previously discovered unclassified values
    if dcnt gt 0 then out_data[dx] = 0
    
    if debug then begin
      out_data_d = byte(out_data * 1.50 + 50)
      if dcnt gt 0 then out_data_d[dx] = 0
      writeu, unit_d, out_data_d
    endif
    writeu, unit, out_data
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  bnames = 'Year.period ' + string(yind, format = '(I04)') + '.' + string(bind + 1, format = '(I02)')
  envi_setup_head, fname = outname $
    , data_type = size(out_data, /type) $
    , /write $
    , interleave = 0 $  ; BSQ
    , nb = nb, nl = nl, ns = ns $
    , bnames = bnames $
    , inherit = meta

  close, unit
  free_lun, unit
  if debug then begin
    envi_setup_head, fname = outname_d $
      , data_type = size(out_data_d, /type) $
      , /write $
      , interleave = 0 $  ; BSQ
      , nb = nb, nl = nl, ns = ns $
      , bnames = bnames $
      , inherit = meta
      
    close, unit_d
    free_lun, unit_d
  endif
  ptr_free, mask
end  

