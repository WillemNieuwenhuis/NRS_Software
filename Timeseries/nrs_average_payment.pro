;+
; :description:
;    Calculate average payment percentage in the growing season only. Do this for each zone separate.
;    The time period is decade (10 day).
;    The average is calculated per year.
;    Assumed is that there is only one growing season and it is uninterupted.
;
; :params:
;    image : in
;      The multi-year timeseries with the payment percentages
;    classfile : in
;      The zone image (classified image where each class is a zone)
;    season_table :
;      Table (binary) with the growing season: for each zone indicates if a decade is part of the growing season 
;
; :keywords:
;    outname : in, optional
;      The name of the output image; if not specified it will be generated
;    start_date : in, required
;      The start date of the input time series
;    end_date : in, required
;      The end date of the time series
;    only_full_year : in, optional, default = yes
;      Only evaluate complete years (from 1 jan to 31 december
;    prog_obj : in, optional
;      A progress indicator object (ProgressBar class)
;    cancelled : out
;      When set, indicates an error, or user abort
;
; :author: nieuwenhuis
;-
pro nrs_average_season_payment, image, classfile, season_table $
  , outname = outname $
  , start_date = start_date $
  , end_date = end_date $
  , only_full_year = only_full_year $
  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  only_full_year = keyword_set(only_full_year) ? only_full_year : 1
  
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

  img_per_year = 36 ; fixed to 10-day periods
  season = nrs_read_csv(season_table, header = header, record_start = 2)
  field_count = n_tags(season)
  if n_elements(img_per_year) eq 0 then img_per_year = field_count - 1 ; adjust to the actual available periods

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
  offset = where(jo[0] eq joy, cnt) ; number of input bands to skip since start of year

  ; calculate the start and end bands for the calculation and the start offset from 1 jan
  first_band = 0    ; first band to evaluate
  last_band = nb - 1  ; last band to evaluate
  season_offset = 0
  first_date = sd
  last_date = ed
  
  jan1_start = julday(1, 1, start_yy)
  if jan1_start lt sd then jan1_start = julday(1, 1, start_yy + 1)
  dec31_end = julday(12, 31, end_yy)
  if dec31_end gt ed then dec31_end = julday(12, 31, end_yy - 1)

  ix = where(jan1_start le jo, ix_cnt)  ;
  year_start = ix[0]
  ix = where(dec31_end gt jo, ix_cnt)
  year_end = ix[-1]

  if only_full_year then begin
    first_date = jan1_start
    last_date = dec31_end
    first_band = year_start
    last_band = year_end
    if jan1_start gt sd then start_yy = start_yy + 1
    if dec31_end lt ed then end_yy = end_yy - 1
  endif


  if first_date gt last_date then begin
    ans = dialog_message('No data available for evaluation', title = 'Error', /error)
    return
  endif
  nr_years = (last_date - first_date + 1) / 365
  nb_eval = last_band - first_band + 1

  ; seasons table contains values zero and one: zero = out of growing season, one = in growing season
  ; Copy the array of struct data into 2-D array
  ; the classes are unsorted in the array, but are assumed to be unique
  classes = season.(0)
  nr_classes = n_elements(classes)
  tbl_data = intarr(nr_classes, field_count - 1)
  for c = 1, field_count - 1 do begin
    tbl_data[*, c - 1] = season.(c)
  endfor

  nrs_set_progress_property, prog_obj, /start, title = 'Calculate averages per growing season'

  ; calculate the season indices
  ; Do this by repeating the grow season for the entire time series
  seas_p1 = bytarr(nb_eval, nr_classes) ; contains one (1) for all bands in the season (per class)
  cnt_p1 = lonarr(nr_classes) ; the length of the season for each class
  lb = last_band mod img_per_year   ; map to the range of number of bands per year
  for cli = 0, nr_classes - 1 do begin
    cl = classes[cli]
    data = reform(tbl_data[cli, *], n_elements(tbl_data[cli, *]), /overwrite) ; remove redundant dimensions
    p1 = reform(rebin(data, img_per_year, nr_years), img_per_year * nr_years)
    if ~only_full_year then begin
      ; add appropriate season info at start and end ...
      ;   ... when the entire time series needs to be handled
      p1 = [data[first_band : -1], p1, data[0 : lb - 1]]
    endif
    seas_p1[*, cli] = p1
    in_season = where(data eq 1, in_seascnt)
    cnt_p1[cli] = in_seascnt
  endfor

  out_undef = !values.f_nan
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_gra', ext = '.dat')
  data_out = fltarr(ns, nl, nr_years) + out_undef

  ; pass through data line by line
  pos = indgen(last_band - first_band + 1) + first_band   ; in case of complete years only
  if ~only_full_year then pos = indgen(nb) ; entire time series

  data = fltarr(ns, n_elements(pos))
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then begin
      return
    endif

    data[*, *] = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    uix = where(data eq undef, ucnt)  ; mark the undefined values, to exclude them from the aggregation
    ; dimension of data is 2D : ns, img_per_year * nr_years
    ; this now matches with seas_p1
    if ucnt gt 0 then data[uix] = !values.f_nan ; exclude undefined values

    for cli = 0, nr_classes - 1 do begin
      cl = classes[cli]
      ix = where(cldata[*, line] eq cl, loc_cnt)  ; select all locations with current class
      if loc_cnt eq 0 then continue

      if cnt_p1[cli] gt 0 then begin
        sp = rebin(transpose(seas_p1[*, cli]), loc_cnt, nb_eval) ; adjust dim to loc_cnt, nb_eval
        select = data[ix, *] * sp  ; dimensions of data[ix, *] and select : loc_cnt, nb_eval
        ; now calculate the average percentage, but only based on the
        ; bands in the growing season
        if only_full_year then begin
          select = reform(select, loc_cnt, img_per_year, nr_years, /overwrite)
          itr = total(select, 2, /nan) / cnt_p1[cli]  ; dimensions of itr : loc_cnt, nr_years
          data_out[ix, line, *] = itr
        endif else begin
          ; Special handling required for the tail and end of the time series
          ; handle the complete years first
          whole = reform(select[ix, first_band : last_band - 1], loc_cnt, img_per_year, nr_years, /overwrite)
          itr = total(whole, 2, /nan) / cnt_p1[cli]
          data_out[ix, line, 1 : -1] = itr
          ; handle head and tail also
          ; head first
          head = select[ix, 0 : first_band - 1] ; dim : loc_cnt, first_band
          itrh = total(head, 2, /nan) / cnt_p1[cli]
          ; tail next
          tail = select[ix, last_band : - 1] ; dim : loc_cnt, nb_eval - last_band
          itrt = total(tail, 2, /nan) / cnt_p1[cli]
          data_out[ix, line, 0] = itrh
          data_out[ix, line, -1] = itrt
        endelse
        
      endif
    endfor
    
  endfor

  meta = envi_set_inheritance(fid, dims, /full)

  yind = indgen(nr_years) + start_yy
  bnames = 'Year ' + string(yind, format = '(I04)')
  envi_write_envi_file, data_out, out_name = outname, bnames = bnames, data_ignore_value = out_undef, inherit = meta
end

pro nrs_average_payment, image, classfile, season_table $
  , img_per_year = img_per_year $
  , outname = outname $
  , start_date = start_date $
  , end_date = end_date $
  , season_origin = season_origin $ ; as date
  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate, obsolete

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
  ; It can detect up to two seasons
  ; seas_p1: The index array contain a one for each decade in the early growth season
  ; seas_p2: The index array contain a one for each decade in the late growth season
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
  envi_write_envi_file, data_out, fname = outname, bnames = bnames, data_ignore_value = undef, inherit = meta, /no_open
end

