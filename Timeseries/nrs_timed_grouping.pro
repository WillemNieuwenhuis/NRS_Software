function nrs_get_interval_mapping, int1, jd1, int2, jd2
  compile_opt idl2, logical_predicate
  
  all_intervals = ['day', '8-day', '10-day', 'bi-monthly', '16-day', 'month', 'year']
  all_indays = [1, 8, 10, 15, 16, 31, 365]
  ix1 = where(strlowcase(int1) eq all_intervals, cnt1)
  ix2 = where(strlowcase(int2) eq all_intervals, cnt1)
  
  num_per = fix(round(all_indays[ix2] / all_indays[ix1]))
  nrs_get_dt_indices, [jd2 - all_indays[ix2], jd2], period = int2, julian = jt
  jd_off = jd1 - jt[-2] ; offset of smallest period in larger period

  return, { num_per : num_per $
          , off_per : jd_off $
          , ind_per : fix(jd_off / all_indays[ix1]) $
          }
end

;+
; :description:
;    Calculate the number of output bands for time-based grouping. For each of the output bands
;    determine the list of input bands required to calculate the aggregate value
;    
; :returns:
;    An array of array pointers. Each item in the array corresponds with an output
;    band and contains exactly one array with the list of associated input bands. 
;
; :params:
;    sd : in, required
;      The start julian date / time
;    ed : in, required
;      The end julian date / time
;    per : in, optional
;      The input interval between bands in days
;    input_period : in, required
;      The input interval between bands as string
;    aggr_interval : in, required
;      The output interval as string; must be the shortest interval if
;      also aggr_interval2 is specified
;    aggr_interval2 : in, optional
;      The optional second output interval as string
;
; :keywords:
;    bnames : out, optional
;      Suggested names for the output bands 
;
; :author: nieuwenhuis
;-
function nrs_get_cross_index, sd, ed, per, input_period, aggr_interval, aggr_interval2, bnames = bnames
  compile_opt idl2, logical_predicate
  
  level2 = n_elements(aggr_interval2) gt 0
  level1 = ~level2 && (n_elements(aggr_interval) gt 0)
  
  ; get the julian day for the input image stack (fractions indicate time of day)
  nrs_get_dt_indices, [sd, ed], period = input_period, julian_out = jul_in $
                    , bins = in_bins, time_mult = in_mult, /start_only

  ; get the indices for the output periods, but ungrouped
  ; shortest period first
  nrs_get_dt_indices, jul_in, period = aggr_interval $
                      , julian_out = jul_out $
                      , indices = indices, ri = rev $
                      , bins = out_bins $   ; for each day of year (DOY) in input give output bin number
                      , time_mult = out_mult $
                      , start_year_index = syi, num_period = npy

  ; always calculate indices per year; needed for level1
  nrs_get_dt_indices, jul_in, period = 'year' $
                      , julian_out = jo_y, /clip $
                      , indices = ind_y, start_year_index = syi_y, num_period = num_y

  ; second period, if any
  if n_elements(aggr_interval2) gt 0 then $
    nrs_get_dt_indices, jul_in, period = aggr_interval2 $
                      , julian_out = jo_lvl2 $
                      , indices = ind_lvl2, start_year_index = syi_lvl2, num_period = num_lvl2

  strput, aggr_interval, strupcase(strmid(aggr_interval, 0, 1)) ; capitalize first character
  
  ; determine all input band indices per output band to be grouped
  ; for 1 level
  if level1 then begin
    if aggr_interval eq 'Year' then begin
      togroup = rev
    endif else begin
      leap_corr = indgen(366)
      leap_corr[60:365] = indgen(365 - 60 + 1) + 61   ; LUT to translate nonleap year doy to leapyear DOY
  
      ; calculate DOY of all input bands (ignoring time, is handled separately below)
      caldat, jul_in, months, days, years
      doy_in = julday(months, days, years) - julday(1, 1, years) + 1
      leap = (((years mod 4) eq 0) and ((years mod 100) ne 0)) or ((years mod 400) eq 0)
      
      ; calculate input band DOY to date index
      doy = doy_in
      nonleap = where(leap eq 0, cnt_leap)
      if cnt_leap gt 0 then doy[nonleap] = leap_corr[doy_in[nonleap]]
      if out_mult gt 1 then begin
        ; prepare sub-day output period lookup
        out_bins = out_bins * out_mult / in_mult ; in_mult must be larger than out_mult!
      endif
  
      togroup = out_bins[doy - 1]
    endelse
    h = histogram(togroup, binsize = 1,  omin = omin, rev = ri) ; the o-vector in ri now contains the groups
    out_periods = n_elements(h)
    p_ar = ptrarr(out_periods)
    for i = 0, n_elements(h) - 1 do begin
      p_ar[i] = ptr_new(ri[ri[i] : ri[i + 1] - 1]) 
    endfor
    
    sy = 1

    nrdig = fix(alog10(out_periods)) + 1
    if npy[0] eq 1 then begin
      caldat, sd, m, d, sy
      form = '("Year: ",i04)'
    endif else if aggr_interval eq 'Month' then form = '("Month: ",i02)' $
    else begin
      form = '("' + aggr_interval + ': ",i0' + string(nrdig, format = '(i0)') + ')'
    endelse
    bnames = string(indgen(out_periods) + sy, format = form)
  endif

  ; for two levels; only year is expected and handled as second level
  if level2 then begin
    num_periods = n_elements(indices)
    indices = [indices, n_elements(jul_in)]  ; append closing position to the array
    p_ar = ptrarr(num_periods)
    for p = 0, num_periods - 1 do begin
      ps = indices[p]
      pe = indices[p + 1]
      arr = indgen(pe - ps) + ps
      if ptr_valid(p_ar[p]) then begin
        ptr_free, p_ar[p]  ; should not come here
      endif
      p_ar[p] = ptr_new(arr)
    endfor

    caldat, jul_out, mm, dd, yy
    st_p = syi lt 0 ? 0 : num_periods - syi
    bn = ((indgen(n_elements(yy)) + st_p) mod num_periods) + 1
    if aggr_interval eq 'Month' then form = '("Year.Month: ",i04,".",i04)' $
    else begin
      bn = jul_out - julday(1, 1, yy) + 1
      form = '("Year.' + aggr_interval + ': ,i03,".",i04)'
    endelse
    bnames = string([transpose(yy), transpose(bn)], format = form)
  endif

  return, p_ar
end

;+
; :Description:
;    Aggregate a timeseries for periods of 8, 10, 16 days, month or year. This also depends
;    on the input date interval (the output interval is always larger).
;     
;    The software can calculate the following aggregations:
;    <ul>
;    <li>Sum
;    <li>Mean
;    <li>Min
;    <li>Max
;    <li>Median
;    </ul> 
;
; :Params:
;    image : in
;      The input timeseries
;    start_date : in
;      The start date of the image
;    end_date : in
;      The end date of the image
;    aggr_function :
;      The aggregation function to execute (sum, mean, minimum, maximum, median, stddev)
;
; :Keywords:
;    outname : in
;      Name of the output result
;    aggr_level1 : in
;      The level 1 aggregation date interval as string (day, 8-day, 16-day, month, year)
;    aggr_level2 : in
;      The level 2 aggregation date interval as string (day, 8-day, 16-day, month, year).
;      If this is the same as level 1 it is ignored.
;    outname : in, optional
;      The output name of the resulting image or image stack.
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :Author: nieuwenhuis
;-
pro nrs_timed_grouping, image $
                   , start_date, end_date $
                   , aggr_func $
                   , aggr_level1 = aggr_interval $
                   , aggr_level2 = aggr_interval2 $
                   , outname = outname $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  ; parameter checking
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif

  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef
  per = nrs_get_period_from_range(sd, ed, nb, per_str = input_period)
  
  nr_levels = 0
  if n_elements(aggr_interval) gt 0 then nr_levels++
  if n_elements(aggr_interval2) gt 0 then nr_levels++
  if nr_levels gt 0 then begin
    all_intervals = ['1 hour', '2 hours', '3 hours', '6 hours', '12 hours', 'day', '8-day', '10-day', 'bi_monthly', '16-day', 'month', 'year']
    inter_ix = where(strlowcase(aggr_interval) eq all_intervals, cnt)
    if cnt eq 0 then begin
      ans = dialog_message('Level 1: Unsupported output period', title = 'Error', /error)
      return
    endif
    if nr_levels gt 1 then begin
      inter_ix2 = where(strlowcase(aggr_interval2) eq all_intervals, cnt2)
      if cnt eq 0 then begin
        ans = dialog_message('Level2: Unsupported output period', title = 'Error', /error)
        return
      endif
      if inter_ix eq inter_ix2 then begin
        void = dialog_message('Duplicate period, ignoring', title = 'Warning')
        nr_levels--
      endif else if inter_ix gt inter_ix2 then begin
        ; swap periods, so level 1 is the shorter period
        h = inter_ix2
        inter_ix2 = inter_ix
        inter_ix = inter_ix2
        h = aggr_interval
        aggr_interval = aggr_interval2
        aggr_interval2 = h
      endif 
    endif
    
    ix_input = where(all_intervals eq input_period)
    if ix_input gt inter_ix then begin
      ans = dialog_message('Nothing to do: output period smaller than input period (use interpolation)', title = 'Information', /Information)
      return
    endif
  endif
  
  all_functions = ['mean', 'stddev', 'sum', 'minimum', 'maximum', 'median']
  func_ix = where(strlowcase(aggr_func) eq all_functions, cnt)
  if cnt eq 0 then begin
    ans = dialog_message('Unsupported aggregation function (' + aggr_func + ')', title = 'Error', /error)
    return
  endif
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_taggr', ext = '.')
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Timeseries aggregation'

  case nr_levels of
    0 : begin
          nb_out = 1
          bnames = [aggr_func]
        end
    1 : begin
          cr_ix = nrs_get_cross_index(sd, ed, per, input_period, aggr_interval, bnames = bnames)
          nb_out = n_elements(cr_ix)
          aggr = fltarr(ns, nb_out)
        end  
    2 : begin
          cr_ix = nrs_get_cross_index(sd, ed, per, input_period, aggr_interval, aggr_interval2, bnames = bnames)
          nb_out = n_elements(cr_ix)
          aggr = fltarr(ns, nb_out)
        end
  endcase

  openw, unit, outname, /get_lun

  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
      file_delete, outname, /allow_nonexist, /quiet
      return
    endif
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    switch nr_levels of
      0 : begin
            dim = size(data, /n_dim)
            case func_ix of
              0 : aggr = mean(data, dimension = dim)
              1 : aggr = stddev(data, dimension = dim)
              2 : aggr = total(data, dim)
              3 : aggr = min(data, dimension = dim)
              4 : aggr = max(data, dimension = dim)
              5 : aggr = median(data, dimension = dim)
            endcase
            break
          end
      1 :
      2 : begin
            for t = 0, n_elements(cr_ix) - 1 do begin
              ix = *(cr_ix[t])
              accu = data[*, ix]
              dim = size(accu, /n_dim)
              case func_ix of
                0 : aggr[*, t] = mean(accu, dimension = dim)
                1 : aggr[*, t] = stddev(accu, dimension = dim)
                2 : aggr[*, t] = total(accu, dim)
                3 : aggr[*, t] = min(accu, dimension = dim)
                4 : aggr[*, t] = max(accu, dimension = dim)
                5 : aggr[*, t] = median(accu, dimension = dim)
              endcase
              
            endfor
          end
    endswitch
    
    writeu, unit, aggr
  endfor
  
  if nr_levels gt 0 then begin
    for t = 0, n_elements(cr_ix) - 1 do begin
      ptr_free, cr_ix[t]
    endfor
  endif

  meta = envi_set_inheritance(fid, dims, /full)
  
  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nb_out, nl = nl, ns = ns $
          , bnames = bnames $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file
end

