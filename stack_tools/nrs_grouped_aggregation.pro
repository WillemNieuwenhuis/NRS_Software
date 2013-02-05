;+
; :Description:
;    Aggregate a timeseries. The aggregation can be done on the complete timeseries,
;    or can be grouped.
;    Aggregation can be done for periods of 8, 10, 16 days, month or year. This also depends
;    on the input date interval (the output interval is always larger).<p> 
;     
;    The software can calculate the following aggregations:
;    <ul>
;    <li>Sum
;    <li>Mean
;    <li>Min
;    <li>Max
;    <li>Standard deviation
;    <li>Median
;    <li>Mode
;    </ul> 
;
; :Params:
;    image : in
;      The input timeseries
;    start_date : in
;      The start date of the image
;    end_date : in
;      The end date of the image
;    aggr_interval : in
;      The aggregation date interval as string (day, 8-day, 16-day, month, year)
;    aggr_function :
;      The aggregation function to execute (sum, mean, minimum, maximum, median, stddev)
;
; :Keywords:
;    grouped : in
;      If set group the data in <i>aggr_interval</i> per year
;    outname : in
;      Name of the output result
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :Author: nieuwenhuis
;-
pro nrs_grouped_aggregation, image $
                   , start_date, end_date $
                   , aggr_interval, aggr_func $
                   , grouped = grouped $
                   , outname = outname $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undef = csy_undef)
  if csy_undef then delvar, mi

  ; parameter checking
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif

  all_intervals = ['day', '8-day', '16-day', 'month', 'year']
  inter_ix = where(strlowcase(aggr_interval) eq all_intervals, cnt)
  if cnt eq 0 then begin
    ans = dialog_message('Unsupported output interval', title = 'Error', /error)
    return
  endif
  
  all_functions = ['mean', 'sum', 'minimum', 'maximum', 'median']
  func_ix = where(strlowcase(aggr_func) eq all_functions, cnt)
  if cnt eq 0 then begin
    ans = dialog_message('Unsupported aggregation function', title = 'Error', /error)
    return
  endif
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_paggr', ext = '.')
  
  doGrouped = keyword_set(grouped)
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Grouped aggregation'

  per_len = [1, 8, 10, 16, 30, 365]
  per_str = ['day', '8-day', '10-day', '16-day', 'month', 'year']
  input_period = (ed - sd + 1) / (nb - 1)   ; in days
  diff = abs(per_len - input_period)
  mn = min(diff, mn_ix)
  input_period = per_str[mn_ix]
  nrs_get_dt_indices, [sd, ed], period = input_period $
                      , julian_out = jul_in, indices = ind_in
                      
  nrs_get_dt_indices, jul_in, period = aggr_interval $
                      , julian_out = jul_out $
                      , indices = indices, start_year_index = syi, num_period = period_len

  nrs_get_dt_indices, jul_out, period = 'year' $
                      , julian_out = jul_year, indices = ind_group

  nb_out = doGrouped ? period_len : n_elements(jul_out)
  openw, unit, outname, /get_lun
  out_data = assoc(unit, fltarr(ns, nb_out))  ; bil

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  output = fltarr(ns, nb_out)
  aggr = fltarr(ns, n_elements(jul_out))
  for l = 1, nl - 1 do begin
    if l mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    for t = 0, n_elements(indices) - 2 do begin
      accu = data[*, indices[t] : indices[t + 1] - 1]
      case func_ix of
        0 : aggr[*, t] = mean(accu, dimension = 2)
        1 : aggr[*, t] = total(accu, 2)
        2 : aggr[*, t] = min(accu, dimension = 2)
        3 : aggr[*, t] = max(accu, dimension = 2)
        4 : aggr[*, t] = median(accu, dimension = 2)
      endcase
    endfor
    if doGrouped then begin
      aggr = shift(aggr, 0, -syi) ; align on 1 jan
      t = 0
      num = n_elements(jul_out)
      while t lt num do begin
        si = t * period_len
        ei = min([num, si + period_len]) - 1
        case func_ix of
          0 : output[*, t] = mean(aggr[si : ei])
          1 : output[*, t] = total(aggr[si : ei])
          2 : output[*, t] = min(aggr[si : ei])
          3 : output[*, t] = max(aggr[si : ei])
          4 : output[*, t] = median(aggr[si : ei])  ; not correct
        endcase
      endwhile
    endif else output = aggr
    
    out_data[l] = output
  endfor

  sn = all_functions[func_ix]
  bn = reform((indgen(period_len * nr_stats) mod nr_stats), 1, period_len * nr_stats)
  dns = stat_names[bn]
  yn = (indgen(period_len * nr_stats) / nr_stats) + 1
  yns = reform(string(yn, format = '(i02)'), 1, period_len * nr_stats)
  bnames = string([yns, dns], format = '("period.stat ",a,".",a)')

  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nb_out, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close assoc
end
