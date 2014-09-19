pro nrs_average_payment, image, classfile, season_table $
  , img_per_year = img_per_year $
  , outname = outname $
  , start_date = start_date $
  , end_date = end_date $
  , season_origin = season_origin $ ; as date
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
  
  ; handle nodata values 
  if n_elements(undef) eq 0 then undef = 0
  if (size(undef, /type) eq 5) and (undef eq 1e-34) then undef = 255
  
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
  dummy = nrs_get_period_from_range(sd, ed, nb, per_str = ip)
  nrs_get_dt_indices, [sd, ed], julian_out = jo, period = ip
  nrs_get_dt_indices, [sdy, ed], julian_out = joy, period = ip
  offset = where(jo[0] eq joy, cnt) ; number of bands to skip since start of year

  ; calculate the start and end bands for the calculation and the start offset from 1 jan
  first_band = 0
  last_band = nb - 1
  season_offset = 0
  first_date = sd
  last_date = ed
  if n_elements(season_origin) gt 0 then begin
    first_date = nrs_str2julian(season_origin)
    caldat, first_date, mm, dd, offset_yy
    last_date = julday(mm, dd - 1, end_yy)  ; last day ending a full season
    if last_date gt ed then begin
      end_yy--
      last_date = julday(mm, dd, end_yy) ; if last day past end of data, move it back a year
    endif
    so_end = where(last_date ge jo, cnt_end)  ; compare with actual available data

    so_data = where(first_date lt jo, cnt)  ; compare with actual available data
    so_jan = where(first_date lt joy, cnt_jan)  ; compare with 1 jan
    if (cnt gt 0) and (cnt_jan gt 0) and (cnt_end gt 0) then begin
      first_band = so_data[0]
      last_band = so_end[-1]
      season_offset = so_jan[0]
    endif
    
  endif
  nr_years = (last_date - first_date) / 365
  
  ; seasons table contains values zero and one: zero = out of growing season, one = in growing season
  ; also shift the data to the appropriate position with respect to the season origin
  ; this is done to align it with the timeseries
  classes = season.(0)
  nr_classes = n_elements(classes)
  tbl_data = intarr(n_elements(season.(0)), field_count - 1)
  for c = 1, field_count - 1 do begin
    tbl_data[*, c - 1] = shift(season.(c), -season_offset)
  endfor
  
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate averages per growing season'

  ; calculate the season indices per calendar year
  seas_p1 = lonarr(img_per_year, nr_years, nr_classes)
  seas_p2 = lonarr(img_per_year, nr_years, nr_classes)
  cnt_p1 = lonarr(nr_classes)
  cnt_p2 = lonarr(nr_classes)
  for cli = 0, nr_classes - 1 do begin
    cl = classes[cli]
    data = [1 - tbl_data[cli, 0], transpose(tbl_data[cli, *]), 1 - tbl_data[cli, -1]]
    diff = (data - shift(data, 1))[1:-1]
    zeroes = where(diff eq -1) - 1
    ones = where(diff eq 1)
    if ones[0] eq img_per_year then continue  ; no grow season detected
    
    if zeroes[0] lt ones[0] then zeroes = zeroes[1:-1]
    len = min([2, n_elements(ones), n_elements(zeroes)])

    p1 = ones[0]
    q1 = zeroes[0]
    seas_p1[p1 : q1, *, cli] = 1
    cnt_p1[cli] = total(seas_p1[*, 0, cli])
    if len gt 1 then begin
      p2 = ones[1]
      q2 = zeroes[1]
      seas_p2[p2 : q2, *, cli] = 1
      cnt_p2[cli] = total(seas_p2[*, 0, cli])
    endif
  endfor
  
  outname = getoutname(image, postfix = '_gra', ext = '.dat')
  data_out = bytarr(ns, nl, 2, nr_years)

  ; pass through data line by line, for complete seasons only
  pos = indgen(last_band - first_band + 2) + first_band
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then begin
      return
    endif

    data_in = envi_get_slice(fid = fid, xs = 0, xe = ns - 1, pos = pos, /bil)
    uix = where(data_in eq undef, ucnt)  ; mark the undefined values, to exclude them from the aggregation
    ; dimension of data = ns * img_per_year * nr_years, so reform it
    data = reform(data_in, ns, img_per_year, nr_years)
    if ucnt gt 0 then data[uix] = !values.f_nan ; exclude undefined values
    
    for cli = 0, nr_classes - 1 do begin
      cl = classes[cli]
      ix = where(cldata[*, line] eq cl, cnt)
      if cnt eq 0 then continue
      
      if cnt_p1[cli] gt 0 then begin
        select = data[ix, *, *] * seas_p1[*, *, cli]
        itr = transpose(mean(select, dimension = 2, /nan))
        data_out[ix, line, 0, *] = rebin(byte(itr), cnt, nr_years)
      endif
      if cnt_p2[cli] gt 0 then begin
        select = data[ix, *, *] * seas_p2[*, *, cli]
        itr = transpose(mean(select, dimension = 2, /nan))
        data_out[ix, line, 1, *] = rebin(byte(itr), cnt, nr_years)
      endif
    endfor
    
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)

  yind = indgen(nr_years * 2) / 2 + start_yy
  bind = indgen(nr_years * 2) mod 2
  bnames = 'Year.period ' + string(yind, format = '(I04)') + '.' + string(bind + 1, format = '(I02)')
  envi_write_envi_file, data_out, fname = outname, bnames = bnames, data_ignore_value = undef, inherit = meta

;  ; define output buffer, init the values to nodata
;  data_out = fltarr(ns * nl, 2) + undef
;  
;  for band = 0, nb - 1 do begin
;    if nrs_update_progress(prog_obj, band, nb, cancelled = cancelled) then begin
;      close, unit
;      free_lun, unit
;      file_delete, outname, /quiet, /noexpand_path
;      return
;    endif
;
;    data = envi_get_data(fid = fid, dims = dims, pos = band)
;    uix = where(data eq undef, ucnt)
;    grow_ix = (band + offset) mod img_per_year
;    grow_yr = (band + offset) / img_per_year
;    correct = grow_yr eq 0 ? offset : 0;
;    for cli = 0, nr_classes - 1 do begin
;      cl = classes[cli]
;      ix = where(cldata eq cl, cnt)
;      if cnt eq 0 then continue
;      
;      ps = seas_start[cli * 2]
;      pe = seas_end[cli * 2]
;      if ps eq -1 then continue ; no growing seasons for this class
;      agg_ix = 0
;      if grow_ix lt ps then continue
;      if grow_ix gt pe then begin
;        agg_ix = 1
;        ps = seas_start[cli * 2 + 1]
;        pe = seas_end[cli * 2 + 1]
;      endif
;      if (grow_ix lt ps) or (grow_ix gt pe) then continue
;      if (grow_yr eq 0) then begin  ; correction if first year does not start 1-jan
;        ps = max([correct, ps])
;        if ps gt pe then continue ; no data available, so skip
;      endif
;      
;      data_out[ix, agg_ix] += 1.0* data[ix] / (pe - ps + 1)
;    endfor
;    
;    if grow_ix eq img_per_year - 1 then begin
;      ; mask out nodata values in input from the output
;      if ucnt gt 0 then data_out[uix] = undef
;      writeu, unit, byte(data_out)
;      data_out[*] = undef ; reset output buffer for next iteration
;    endif
;  endfor
;
;  meta = envi_set_inheritance(fid, dims, /full)
;  
;  yind = indgen(nr_years * 2) / 2 + start_yy
;  bind = indgen(nr_years * 2) mod 2
;  bnames = 'Year.period ' + string(yind, format = '(I04)') + '.' + string(bind + 1, format = '(I02)')
;  envi_setup_head, fname = outname $
;    , data_type = size(byte(1), /type) $
;    , /write $
;    , interleave = 0 $  ; BSQ
;    , nb = nr_years * 2, nl = nl, ns = ns $
;    , bnames = bnames $
;    , inherit = meta
;    
;  close, unit
;  free_lun, unit
  
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