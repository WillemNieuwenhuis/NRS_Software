;+
; :Description:
;    Calculate all month and year boundaries in day numbers
;
; :Params:
;    julian : in, required
;      Array of dates/times in julian days 
;    interval : in, required
;      The output date/time interval between consecutive observations in seconds
;
; :Keywords:
;    nr_obs : out
;      The number of observations in the new timeseries
;    dmbands : out
;      array containing the number of days in each month from start date to end date
;    dybands : out
;      array containing the number of days in each year from start date to end date
;    doy : out
;      Array of days since the start of year for all of the input dates (1 jan == 1) 
;
; :Author: nieuwenhuis
; :Obsolete:
;-
pro nrs_get_days_indices, julian, interval, nr_obs = nr_obs, dmbands = dmbands, dybands = dybands, doy = doy
  compile_opt idl2, logical_predicate
  
  caldat, julian, mm, md, my, mh, mmn, ms
  start_year = min(my, max = end_year)

  nb = n_elements(julian)
  
  ; calculate day of year
  uq = my[uniq(my)]
  doy = intarr(nb)
  for u = 0, n_elements(uq) - 1 do begin
    ix = where(my eq uq[u])
    juluq = julday(1, 1, uq[u], 0, 0, 0)
    doy[ix] = julian[ix] - juluq + 1
  endfor
  
  sy = my[0]
  sm = mm[0]
  ey = my[nb - 1]
  em = mm[nb - 1]
  mcnt = (ey - sy) * 12 + (em - sm) + 1
  mar = ((indgen(mcnt + 1) + sm - 1) mod 12) + 1
  yar = ((indgen(mcnt + 1) + sm - 1) / 12) + sy
  
  ; calculate days per month
  ymjul = julday(mar, 1, yar)
  ymshft = shift(ymjul, 1)
  dom = shift(fix(ymjul - ymshft), -1)
  dmbands = dom[0 : mcnt - 1]
  
  ; calculate days per year
  uq = indgen(ey - sy + 1) + sy
  nbpy = n_elements(uq)
  uq = [uq, uq[nbpy -1] + 1]
  ydjul = julday(1, 1, uq)
  ydshft = shift(ydjul, 1)
  dpy = shift(fix(ydjul - ydshft), -1)
  dybands = dpy[0 : nbpy - 1]

end

;+
; :description:
;    Determine the time interval between bands in days
;
; :params:
;    sd : in, required
;      Start julian date / time
;    ed : in, required
;      End julian date / time
;    nb : in, required
;      The number of bands between the start and end date / time
;
; :returns:
;   The interval in days; interval less than one day are indicated as a fraction
;
; :keywords:
;    per_str : out
;      The interval expressed as string
;
; :author: nieuwenhuis
;-
function nrs_get_period_from_range, sd, ed, nb, per_str = input_period
  compile_opt idl2, logical_predicate
  
  per_len = [0.0416667, 0.083333, 0.125, 0.25, 0.5, 1, 8, 10, 15, 16, 30, 365]
  per_str = ['1 hour', '2 hours', '3 hours', '6 hours', '12 hours', 'day', '8-day', '10-day', 'bi-monthly', '16-day', 'month', 'year']
  input_period = (ed - sd + 1) / (nb - 1)   ; in days
  diff = abs(per_len - input_period)
  mn = min(diff, mn_ix)
  input_period = per_str[mn_ix]
  
  return, per_len[mn_ix]
end

pro nrs_days_per_year, julian, dybands = dybands, actbands = actbands
  compile_opt idl2, logical_predicate
  
  jul = julian[sort(julian)]
  caldat, jul, mm, md, my

  nb = n_elements(jul)
  sy = my[0]
  ey = my[nb - 1]

  uq = indgen(ey - sy + 1) + sy
  nbpy = n_elements(uq)
  uq = [uq, uq[nbpy -1] + 1]
  
  ; total days in the observed years
  ydjul = julday(1, 1, uq)
  ydshft = shift(ydjul, 1)
  dpy = shift(fix(ydjul - ydshft), -1)
  dybands = dpy[0 : nbpy - 1]
  
  ; actual days in the observed years
  actbands = dybands
  actbands[0] -= (julday(mm[0], md[0], sy) - julday(1, 1, sy))
  actbands[nbpy - 1] -= (julday(1, 1, ey + 1) - julday(mm[nb - 1], md[nb - 1], ey))
end

pro nrs_days_per_month, julian, dmbands = dmbands, actbands = actbands
  compile_opt idl2, logical_predicate
  
  caldat, julian, mm, md, my, mh, mmn, ms
  
  nb = n_elements(my)
  sy = my[0]
  sm = mm[0]
  ey = my[nb - 1]
  em = mm[nb - 1]
  mcnt = (ey - sy) * 12 + (em - sm) + 1
  mar = ((indgen(mcnt + 1) + sm - 1) mod 12) + 1
  yar = ((indgen(mcnt + 1) + sm - 1) / 12) + sy
  
  ; calculate days per month
  ymjul = julday(mar, 1, yar, 0, 0, 0)
  ymshft = shift(ymjul, 1)
  dom = shift(fix(ymjul - ymshft), -1)
  dmbands = dom[0 : mcnt - 1]
  
  ; actual days in the observed months
  actbands = dmbands
  actbands[0] -= (julday(mm[0], md[0], sy, 0, 0, 0) - julday(mm[0], 1, sy, 0, 0, 0))
  actbands[mcnt - 1] = md[nb - 1] - 1
end

;+
; :Description:
;    Find the index in target for all values in source. The index for a value V from source
;    is defined as the first position in the target array with value W where W >= V.<p>
;    The arrays don't have to be of the same length. The arrays are assumed to be
;    ordered in increasing values.
;    
; :Params:
;    target : in, required
;      First array 
;    source : in, required
;      Second array
;      
; :Keywords:
;    index : out, optional
;      Indices into the target array for all values in the source array.
;    ri : out, optional
;      Reverse indices for a lookup from target in source. Indices into
;      the source array for all values in the target array.
;
; :History:
;   - 29 oct 2013: nieuwenhuis, added aligned keyword 
;   - dec 2012: Added ri keyword
;   - nov 2011: creation
;
; :Author: nieuwenhuis
;-
pro nrs_match_arrays, target, source, index = index, ri = ri
  compile_opt idl2

  ti = 0
  se = n_elements(source)
  te = n_elements(target)
  index = lonarr(se)
  if te eq 2 then begin
    ; special case
    index[0 : se/2] = 0
    index[se/2 + 1 : -1] = 1
  endif else begin
    for si = 0, se - 1 do begin
      while ti lt te && source[si] gt target[ti] do begin
        ti++
      endwhile
      index[si] = ti lt te ? ti : -1
    endfor
  endelse
  
  if arg_present(ri) then begin
    ri = lonarr(te)
    si = 0
    for ti = 0, te - 1 do begin
      while si lt se && target[ti] ge source[si] do begin
        si++
      endwhile
      ri[ti] = si le se ? si - 1 : -1
    endfor
  endif
end

;+
; :description:
;    Describe the procedure.
;
; :params:
;    sy
;    ey
;    nb
;
; :author: nieuwenhuis
; :obsolete:
;-
;function nrs_map_date, sy, ey, nb
;  jd = [julday(1, 1, sy), julday(12, 31, ey)]
;  per = nrs_get_period_from_range(jd[0], jd[1], nb, per_str = per_str)
;  nrs_get_dt_indices, jd, period = 'day', julian_out = jo
;  nrs_get_dt_indices, jo, period = per_str, indices = ind, ri = ri, /clip
;
;  return, ri
;end

;+
; :Description:
;    Calculate equidistant distributed dates in the range determined by the
;    list of <i>julian</i> dates. For each year the counting start at 1 jan. The distance
;    between two dates is user specified in <i>period</i>. The number of dates
;    per year is also user specified: <i>periods_per_year</i> (the product 
;    <i>period</i> * <i>periods_per_year</i> is not guaranteed to be 365/366). The
;    first and last dates are cropped to the input dates in <i>julian</i>
;
; :Params:
;    julian : in
;      The list of dates sorted in increasing order. It should contain at least two dates
;    period : in
;      Period in days
;    periods_per_year : in
;      Number of dates
;
; :Keywords:
;    jul_out : out
;      The list of calculated dates
;    crop : in
;      Crop the range to include / replace the start and end days; this is the default
;      It overrules <i>clip</i>
;    clip : in
;      Clip the range by only fitting calculated days within the input range. the start and end
;      days are not added or replacing the start / end positions. Overruled by <i>crop</i>
;    start_only : in
;      Include only the start dates of each period. If set this will overrule crop and clip
;
; :Author: nieuwenhuis
;-
pro nrs_get_days_from, julian, period, periods_per_year, jul_out = jul_out $
                     , crop = crop $
                     , clip = clip $
                     , start_only = start_only
  compile_opt idl2, logical_predicate

  dostart = keyword_set(start_only)
  docrop = keyword_set(crop)
  doclip = ~dostart and ~docrop and keyword_set(clip)
  docrop = ~dostart and (docrop or ~doclip)
  
  nb = n_elements(julian)
  caldat, julian[[0, nb - 1]], mm, dd, yy
  sy = yy[0]
  ey = yy[1]
  num = ey - sy + 2
  year_ix = rebin(indgen(num), periods_per_year * num)
  day_ix = (indgen(num * periods_per_year) mod periods_per_year) * period
  jds_11 = julday(1, 1, indgen(num) + sy)
  dss = jds_11 - jds_11[0]  ; day number of 1 jan of each year since start
  jul_n = dss[year_ix] + day_ix + jds_11[0]
  sx = where(julian[0] ge jul_n, s_cnt)
  ex = where(julian[nb - 1] le jul_n, e_cnt)
  sx = s_cnt eq 0 ? 0 : sx[s_cnt - 1]
  ex = e_cnt eq 0 ? n_elements(jul_n) : ex[0]
  if doclip then ex = ex - 1
  if dostart then ex = ex - 1
  jul_out = jul_n[sx : ex]
  new_nb = n_elements(jul_out)
  if docrop then jul_out[[0, new_nb - 1]] = julian[[0, nb - 1]] ; crop
end

;+
; :Description:
;    Calculate number of observations / layers in a new timeseries, running
;    from the first date to the last date in the input array 'julian' with
;    an user specified date/time 'interval'.
;    
; :Params:
;    julian : in, required
;      Array of dates/times in julian days to redivide. The array is assumed to be ordered older date/times first
;
; :Keywords:
;    interval : in
;      The output date/time interval between consecutive observations in days;
;      if specified, takes preference over the <i>period</i> keyword
;    period : in
;      The output date/time interval as a string; possible options:
;      <ul>
;      <li>day  : List of all days between first and last input date
;      <li>8-day  : List of all days 8 days apart: restart counting days each year at 1 january. The
;                    first and last day are cropped to the input date range
;      <li>10-day : List of all days 10 days apart: restart counting at the first of each month
;                   , with each month containing three dates (last periods takes the rest of the month)
;      <li>16-day  : List of all days 16 days apart: restart counting days each year at 1 january. The
;                    first and last day are cropped to the input date range
;      <li>month  : List of dates at the first of each month
;      <li>year : List of dates at the start of each year
;      </ul>
;      The keyword is ignored if the <i>interval</i> keyword is specified
;    julian_out : out
;      The output (starting) dates as julian dates
;    indices : out
;      List of indices into the input array. The indices match date/time in output with input
;    bins : out
;      List of indices: For an output period of one day or larger it contains for each day of year
;      the corresponding output time period; the bins list contains 366 indices.
;      <p>    
;      For sub-day output periods it contains for each day of year the corresponding output
;      time period; it contains 366 times the number of sub-periods indices. The index in the array
;      is calculated as DOY x number of sub-day periods + sub-day period index.
;      <p>
;      Note that the number of indices in this list does not need to match the number of output periods!
;    time_mult : out
;      This is used with the bins array. For output periods larger than or equal to a day this will
;      be set to one, indicating the time resolution of bins is one day.
;      <p>
;      It will be set to a larger value than one in case the output time resolution is smaller than
;      one day. In that case this keyword is set to the number of time periods per day.
;    start_year_index : out
;      Index of the first occurrence of 1 jan in the list of output dates;
;      is -1 if 1 jan is not in the dates
;    offset : out
;      Indicate the distance to the first date in the first complete period
;      from the start date of the range
;    num_period : out
;      Output number of periods per year
;    period_start : in
;      Include only the start dates of each period. If set this will overrule crop and clip
;    crop : in
;      Crop the range to include / replace the start and end days; this is the default
;      It overrules <i>clip</i>
;    clip : in
;      Clip the range by only fitting calculated days within the range. the start and end
;      days are not added / replacing the start / end positions. Overruled by <i>crop</i>

; :Author: nieuwenhuis
; 
; :History:
;    - 29 oct 2013: nieuwenhuis, added support for smaller than one day periods
;    - jan, 2012: Inception
;-
pro nrs_get_dt_indices, julian, interval = interval, period = period $
                      , julian_out = jul_out $
                      , crop = crop $
                      , clip = clip $
                      , start_only = start_only $
                      , indices = indices $
                      , bins = bins, time_mult = mult $
                      , ri = ri $
                      , start_year_index = start_year_index $
                      , offset = offset $
                      , num_period = period_out
  compile_opt idl2, logical_predicate

  dostart = keyword_set(start_only)
  docrop = keyword_set(crop)
  doclip = ~dostart and ~docrop and keyword_set(clip)
  docrop = ~dostart and (docrop or ~doclip)

  caldat, julian, mm, md, my, mh, mmn, ms
  
  nb = n_elements(julian)
  in_period = (julian[-1] - julian[0]) / nb 
  
  sy = my[0]
  sm = mm[0]
  sd = md[0]
  ey = my[nb - 1]
  em = mm[nb - 1]
  ed = md[nb - 1]
  th = mh[0]
  tm = mmn[0]
  ts = ms[0]
  year_cnt = ey - sy + 1
  month_cnt = (ey - sy) * 12 + (em - sm) + 1
  
  if n_elements(period) gt 0 then period_in = period
  mult = 1
  if n_elements(interval) gt 0 then begin
    all_per = [1, 8, 10, 15, 16, 30, 365]
    all_int = ['day', '8-day', '10-day', 'bi-monthly', '16-day', 'month', 'year']
    ix = where(interval eq  all_per, cnt)
    if cnt eq 0 then begin
      jul_out = []
      return
    endif
    period = all_int[ix]
  endif
  timar = ['1 hour', '2 hours', '3 hours', '4 hours', '6 hours', '12 hours']
  mular = [24, 12, 8, 6, 4, 2]
  ix = where(period eq timar, cnt_tim)
  if cnt_tim eq 1 then begin
    period_in = 'time'
    mult = (mular[ix])[0]
  endif else period_in = period
  case strlowcase(period_in) of
    'time'   : begin
                 nrs_days_per_year, [julian[0], julian[-1]], dybands = period_out
                 new_nb = long( (julian[nb - 1] - julian[0]) * mult + 1)
                 jul_out = julday(sm, sd, sy, th, tm, ts) + findgen(new_nb) / mult
                 bins = lindgen(366 * mult)
               end
    'day'    : begin
                 nrs_days_per_year, [julian[0], julian[-1]], dybands = period_out
                 new_nb = long(julian[nb - 1] - julian[0]) + 1
                 jul_out = julday(sm, sd, sy) + lindgen(new_nb)
                 bins = lindgen(366) + 1
               end
    '8-day'  : begin
                 period_out = 46 
                 nrs_get_days_from, julian, 8, 46, jul_out = jul_out, start_only = dostart, crop = docrop, clip = doclip
                 bins = (lindgen(366) / 8) + 1
               end
    '10-day' : begin
                 period_out = 36 
                 corr = (sd - 1) / 10
                 dar = (corr + (indgen(month_cnt * 3))) mod 3 * 10 + 1
                 dar[0] = sd
                 mar = 1 + (((corr + indgen(month_cnt * 3)) / 3) + (sm - 1)) mod 12
                 yar = sy + (((corr + indgen(month_cnt * 3)) / 3) + (sm - 1)) / 12
                 jul_out = julday(mar, dar, yar)
                 bins = [10, 10, 11, 10, 10, 9, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10 $
                       , 10, 10, 11, 10, 10, 11, 10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 11]
                 h = histogram(total(bins, /cum) - 1, /binsize, rev = ri, min = 0)
                 bins = ri[0 : n_elements(h) - 1] - ri[0]
               end
    'bi-monthly' : begin
                 period_out = 24
                 corr = (sd - 1) / 10
                 dar = (corr + (indgen(month_cnt * 2))) mod 2 * 15 + 1
                 dar[0] = sd
                 mar = 1 + (((corr + indgen(month_cnt * 2)) / 2) + (sm - 1)) mod 12
                 yar = sy + (((corr + indgen(month_cnt * 2)) / 2) + (sm - 1)) / 12
                 jul_out = julday(mar, dar, yar)
                 bins = [15, 16, 15, 14, 15, 16, 15, 15, 15, 16, 15, 15, 15, 16, 15, 16, 15, 15, 15, 16, 15, 15, 15, 16]
                 h = histogram(total(bins, /cum) - 1, /binsize, rev = ri, min = 0)
                 bins = ri[0 : n_elements(h) - 1] - ri[0]
               end
    '16-day' : begin
                period_out = 23 
                nrs_get_days_from, julian, 16, 23, jul_out = jul_out, start_only = dostart, crop = docrop, clip = doclip
                bins = (lindgen(366) / 16) + 1
              end
    'month' : begin
                period_out = 12
                if docrop and (ed gt 1) then month_cnt++
                dar = intarr(month_cnt) + 1
                mar = 1 + (indgen(month_cnt) + (sm - 1)) mod 12
                yar = sy + (indgen(month_cnt) + (sm - 1)) / 12
                if docrop and (ed gt 1) then begin
                  dar[[0, month_cnt - 1]] = [sd, ed]
                  mar[[0, month_cnt - 1]] = [sm, em]
                  yar[[0, month_cnt - 1]] = [sy, ey]
                endif 
                jul_out = julday(mar, dar, yar)
                bins = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                h = histogram(total(bins, /cum) - 1, /binsize, rev = ri, min = 0)
                bins = ri[0 : n_elements(h) - 1] - ri[0]
              end
;    '3-month' : begin
;                mar = 1 + (indgen(month_cnt) + (sm - 1)) mod 12
;                yar = sy + (indgen(month_cnt) + (sm - 1)) / 12
;                jul_out = julday(mar, 1, yar)
;              end
    'year'  : begin
                period_out = 1
                if docrop and (ed gt 1) then year_cnt++
                jul_out = julday(1, 1, sy + indgen(year_cnt))
                bins = lonarr(366)
              end
  endcase
  
  ; Determine types of the dates:
  ; long(3) for dates, or double(5) for date and time
  tp_out = size(jul_out, /type)
  tp_in = size(julian, /type)
  if tp_in ne tp_out && tp_in eq 5 then begin
    ji = long(julian + 0.5D)
  endif else begin
    ji = julian
  endelse

  start_year_index = -1
  y1 = where(julday(1, 1, sy) eq jul_out, cnt1)
  y2 = where(julday(1, 1, sy + 1) eq jul_out, cnt2)
  if cnt1 gt 0 then start_year_index = y1[0] $
  else if cnt2 gt 0 then start_year_index = y2[0]
  
  if arg_present(offset) then begin
    offset = jul_out[1] - julian[0]
  endif
  
  if arg_present(ri) then begin
    nrs_match_arrays, ji, jul_out, index = indices, ri = ri
  endif else begin
    nrs_match_arrays, ji, jul_out, index = indices
  endelse
end

;+
; :description:
;    Determine the output bin number for each input band
;
; :params:
;    julian
;    period
;
; :keywords:
;    groups
;
; :author: nieuwenhuis
;-
pro nrs_get_groups, julian, period, groups = groups
  compile_opt idl2, logical_predicate

  docrop = keyword_set(crop)
  doclip = ~docrop and keyword_set(clip)
  docrop = docrop or ~doclip
  
  dpy = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  leap_corr = indgen(366)
  leap_corr[60:365] = indgen(365 - 60 + 1) + 61   ; LUT to translate nonleap year doy to leapyear doy
  caldat, julian, mm, md, my, mh, mmn, ms
  

  nb = n_elements(julian)
  in_period = (julian[-1] - julian[0]) / nb

  sy = my[0]
  sm = mm[0]
  sd = md[0]
  ey = my[nb - 1]
  em = mm[nb - 1]
  ed = md[nb - 1]
  th = mh[0]
  tm = mmn[0]
  ts = ms[0]
  year_cnt = ey - sy + 1
  month_cnt = (ey - sy) * 12 + (em - sm) + 1

  case strlowcase(period) of
;    'time'   : begin
;      nrs_days_per_year, [julian[0], julian[-1]], dybands = period_out
;      new_nb = (long(julian[nb - 1] - julian[0]) + 1) * mult
;      jul_out = julday(sm, sd, sy, th, tm, ts) + findgen(new_nb) / mult
;    end
    'day'    : begin
      nrs_days_per_year, [julian[0], julian[-1]], dybands = period_out
      new_nb = long(julian[nb - 1] - julian[0]) + 1
      jul_out = julday(sm, sd, sy) + lindgen(new_nb)
    end
    '8-day'  : begin
      period_out = 46
      nrs_get_days_from, julian, 8, 46, jul_out = jul_out, crop = crop, clip = clip
    end
    '10-day' : begin
      period_out = 36
      corr = (sd - 1) / 10
      dar = (corr + (indgen(month_cnt * 3))) mod 3 * 10 + 1
      dar[0] = sd
      mar = 1 + (((corr + indgen(month_cnt * 3)) / 3) + (sm - 1)) mod 12
      yar = sy + (((corr + indgen(month_cnt * 3)) / 3) + (sm - 1)) / 12
      jul_out = julday(mar, dar, yar)
    end
    'bi-monthly' : begin
      period_out = 24
      corr = (sd - 1) / 10
      dar = (corr + (indgen(month_cnt * 2))) mod 2 * 15 + 1
      dar[0] = sd
      mar = 1 + (((corr + indgen(month_cnt * 2)) / 2) + (sm - 1)) mod 12
      yar = sy + (((corr + indgen(month_cnt * 2)) / 2) + (sm - 1)) / 12
      jul_out = julday(mar, dar, yar)
    end
    '16-day' : begin
      period_out = 23
      nrs_get_days_from, julian, 16, 23, jul_out = jul_out, crop = crop, clip = clip
    end
    'month' : begin
      period_out = 12
      if docrop and (ed gt 1) then month_cnt++
      dar = intarr(month_cnt) + 1
      mar = 1 + (indgen(month_cnt) + (sm - 1)) mod 12
      yar = sy + (indgen(month_cnt) + (sm - 1)) / 12
      if docrop and (ed gt 1) then begin
        dar[[0, month_cnt - 1]] = [sd, ed]
        mar[[0, month_cnt - 1]] = [sm, em]
        yar[[0, month_cnt - 1]] = [sy, ey]
      endif
      jul_out = julday(mar, dar, yar)
    end
    ;      '3-month' : begin
    ;                  mar = 1 + (indgen(month_cnt) + (sm - 1)) mod 12
    ;                  yar = sy + (indgen(month_cnt) + (sm - 1)) / 12
    ;                  jul_out = julday(mar, 1, yar)
    ;                end
    'year'  : begin
      period_out = 1
      if docrop and (ed gt 1) then year_cnt++
      jul_out = julday(1, 1, sy + indgen(year_cnt))
      jul_out[[0,year_cnt - 1]] = julian[[0, nb - 1]]
    end
  endcase
end
