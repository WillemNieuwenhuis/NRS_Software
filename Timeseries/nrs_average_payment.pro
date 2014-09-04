pro nrs_average_payment, image, classfile, season_table $
  , img_per_year = img_per_year $
  , outname = outname $
  , start_date = start_date $
  , end_date = end_date $
  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  if n_params() ne 3 then begin
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
  
  season = nrs_read_csv(season_table, header = header, record_start = 2)
  field_count = n_tags(season)
  if n_elements(img_per_year) eq 0 then img_per_year = field_count - 1 ; don't count the start column header
  
  cancelled = 0
  
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass $
    , has_unclassified = has_unclassified $
    , class_adjust = 0
    
  caldat, sd, mm, dd, start_yy
  caldat, ed, mm, dd, end_yy 
  sdy = julday(1, 1, start_yy)
  nrs_get_dt_indices, [sd, ed], julian_out = jo, period = '10-day'
  nrs_get_dt_indices, [sdy, ed], julian_out = joy, period = '10-day'
  offset = where(jo[0] eq joy, cnt) ; number of bands to skip since start of year
  nr_years = end_yy - start_yy + 1
  
  ; seasons table contains values zero and one: zero = out of growing season, one = in growing season
  classes = season.(0)
  nr_classes = n_elements(classes)
  tbl_data = intarr(n_elements(season.(0)), field_count - 1)
  for c = 1, field_count - 1 do begin
    tbl_data[*, c - 1] = season.(c)
  endfor
  
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate averages per growing seasons'

  seas_start = intarr(nr_classes * 2) - 1
  seas_end   = intarr(nr_classes * 2) - 1
  for cli = 0, nr_classes - 1 do begin
    cl = classes[cli]
    data = [1 - tbl_data[cli, 0], transpose(tbl_data[cli, *]), 1 - tbl_data[cli, -1]]
    diff = (data - shift(data, 1))[1:-1]
    zeroes = where(diff eq -1) - 1
    ones = where(diff eq 1)
    if ones[0] eq img_per_year then continue
    if zeroes[0] lt ones[0] then zeroes = zeroes[1:-1]
    len = min([2, n_elements(ones), n_elements(zeroes)])
    seas_start[cli * 2, 0] = ones[0 : len - 1]
    seas_end[cli * 2, 0] = zeroes[0 : len -1]
  endfor
  
  outname_d = getoutname(image, postfix = '_gra', ext = '.dat')
  openw, unit, outname, /get_lun

  data_out = bytarr(ns * nl, 2)
  
  for band = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, band, nb, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
      file_delete, outname, /quiet, /noexpand_path
      return
    endif

    data = envi_get_data(fid = fid, dims = dims, pos = band)
    grow_ix = (band + offset) mod img_per_year
    grow_yr = (band + offset) / img_per_year
    correct = grow_yr eq 0 ? offset : 0;
    for cli = 0, nr_classes - 1 do begin
      cl = classes[cli]
      ix = where(cldata eq cl, cnt)
      if cnt eq 0 then continue
      
      ps = seas_start[cli * 2, 0]
      pe = seas_end[cli * 2, 0]
      agg_ix = 0
      if grow_ix lt ps then continue
      if grow_ix gt pe then begin
        agg_ix = 1
        ps = seas_start[cli * 2, 1]
        pe = seas_end[cli * 2, 1]
      endif
      if (grow_ix lt ps) or (grow_ix gt pe) then continue
      if (grow_yr eq 0) then begin  ; correction if first year does not start 1-jan
        ps = max([correct, ps])
        if ps gt pe then continue ; no data available, so skip
      endif
      
      data_out[ix, agg_ix] += data[ix] / (pe - ps + 1)
    endfor
    
    if grow_ix eq img_per_year - 1 then begin
      writeu, unit, data_out
      data_out[*] = 0
    endif
  endfor

  meta = envi_set_inheritance(fid, dims, /full)
  
  yind = indgen(nr_years * 2) / 2 + sdy
  bind = indgen(nr_years * 2) mod 2
  bnames = 'Year.period ' + string(yind, format = '(I04)') + '.' + string(bind + 1, format = '(I02)')
  envi_setup_head, fname = outname $
    , data_type = size(data_out, /type) $
    , /write $
    , interleave = 0 $  ; BSQ
    , nb = nr_years * 2, nl = nl, ns = ns $
    , bnames = bnames $
    , inherit = meta
    
  close, unit
  free_lun, unit
  
end

pro tttt_sub, aa
  compile_opt idl2
  
  print,aa, format = '(36i2)'
  aa = [1-aa[0], aa, 1-aa[-1]]
  diff = aa - shift(aa, 1)
  print,aa, format = '(38i2)'
  print, diff, format = '(38i2)'
  diff = diff[1:-1]
  os = where(diff eq 1)
  zs = where(diff eq -1)
  print,diff, format = '(37i2)'
  print,zs,os
  if zs[0] lt os[0] then zs = zs[1:-1] - 1 
  print,zs,os
  
end
  
pro tttt_ap
  compile_opt idl2
  
  aa=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]
  tttt_sub, aa

  aa=[0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]
  tttt_sub, aa

  aa=[1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]
  tttt_sub, aa
  
  aa=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1]
  tttt_sub, aa
  
  aa=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]
  tttt_sub, aa
  
end