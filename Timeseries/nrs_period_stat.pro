;+
; :Description:
;    Calculate statistics in a timeseries. Given that the timeseries is a certain
;    number of periods, each divided into the same number of subperiods the statistics
;    are determined for each subperiod in all periods (f.e. with year period and monthly
;    subperiods: calculate the stats for each separate month in all years).<p>
;    The software calculates 5 statistics:
;    <ol>
;    <li>Mean
;    <li>Min
;    <li>Max
;    <li>Standard deviation
;    <li>Median
;    </ol> 
;    The number of layers generated is the period_len * 5.
;
; :Params:
;    image : in
;      The input timeseries
;    period_len :
;      The number of subperiods per period (f.e. 12 for months/year). The number of periods
;      is then calculated automatically
;
; :Keywords:
;    outname : in
;      Name of the output result
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :Author: nieuwenhuis
;-
pro nrs_period_stat, image, period_len, outname = outname $
                   , mean = mean, min = min, max = max, stddev = stddev, median = median, all = all $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undef = csy_undef)
  if csy_undef then delvar, mi
  
  if n_elements(period_len) le 0 then return

  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_pstat', ext = '.')
  
  cancelled = 0
  
  doAll = keyword_set(all)
  doMean = keyword_set(mean)
  doMin  = keyword_set(min)
  doMax  = keyword_set(max)
  doStd  = keyword_set(stddev)
  doMed  = keyword_set(median)
  
  do_arr = [doMean, doMin, doMax, doStd, doMed]
  if doAll then do_arr = intarr(n_elements(do_arr)) + 1
  do_ix = where(do_arr eq 1, cix)
  if cix eq 0 then begin
    ans = dialog_message('Nothing to do, quitting', title = 'Information', /information)
    return
  endif
  do_rix = fix(total(do_arr, /cum)) - 1
  
  nrs_set_progress_property, prog_obj, /start, title = 'Periodic statistics'

  nr_periods = nb / period_len
  nr_stats = total(do_arr)
  openw, unit, outname, /get_lun
  out_data = assoc(unit, fltarr(ns, nr_stats, period_len))  ; bil

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, prog_obj
    cancelled = 1
    return
  endif
  
  output = fltarr(ns, nr_stats, period_len)
  for l = 1, nl - 1 do begin
    if l mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    data = reform(data, ns, period_len, nr_periods, /overwrite)
    mom = mean(data, dimension = 3)
    mx = max(data, min = mn, dimension = 3)
    if nr_periods gt 1 then begin
      sdev = stddev(data, dimension = 3)
      med = median(data, dimension = 3)
    endif else begin
      sdev = fltarr(ns, period_len)
      med = temporary(data)
    endelse
    if do_arr[0] then output[*, do_rix[0], *] = mom[*, *]    ; mean
    if do_arr[1] then output[*, do_rix[1], *] = mn[*, *]     ; min
    if do_arr[2] then output[*, do_rix[2], *] = mx[*, *]     ; max
    if do_arr[3] then output[*, do_rix[3], *] = sdev         ; std dev
    if do_arr[4] then output[*, do_rix[4], *] = med          ; median
    
    out_data[l] = output
  endfor
  
  sn = ['Mean', 'Min', 'Max', 'Stddev', 'Median']
  stat_names = sn[do_ix]
  bn = reform((indgen(period_len * nr_stats) mod nr_stats), 1, period_len * nr_stats)
  dns = stat_names[bn]
  yn = (indgen(period_len * nr_stats) / nr_stats) + 1
  yns = reform(string(yn, format = '(i02)'), 1, period_len * nr_stats)
  bnames = string([yns, dns], format = '("period.stat ",a,".",a)')

  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , /write $
          , interleave = 1 $  ; BIL
          , nb = period_len * nr_stats, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close assoc
end
