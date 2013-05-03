function nrs_get_cross_index, sd, ed, per, input_period, aggr_interval, aggr_interval2
  compile_opt idl2, logical_predicate
  
  ; get the indices for the input image stack
  nrs_get_dt_indices, [sd, ed], period = input_period $
                      , julian_out = jul_in, indices = ind_in
                      
  ; get the indices for the output image stack
  ; shortest period first
  nrs_get_dt_indices, jul_in, period = aggr_interval $
                      , julian_out = jul_out $
                      , indices = indices, start_year_index = syi, num_period = num_periods

  ; second period, if any
  nrs_get_dt_indices, jul_in, period = aggr_interval2 $
                      , julian_out = jo_lvl2 $
                      , indices = ind_lvl2, start_year_index = syi_lvl2, num_period = num_lvl2

  ; Always calculate year indices
  nrs_get_dt_indices, jul_in, period = 'year' $
                      , julian_out = jul_yy $
                      , indices = ind_yy

  ;
  p_ar = ptrarr(num_periods)
  offset = jul_out[1] - sd
  pix = syi lt 0 ? 0 : num_periods - syi
  for p = 0, num_periods - 1 do begin
    arr = []
    six = p
    while six lt n_elements(indices) - 1 do begin
      arr = [arr, indgen(indices[six + 1] - indices[six]) + indices[six]]
      six += num_periods
    endwhile
    ppix = (pix + p) mod num_periods
    if ptr_valid(p_ar[ppix]) then ptr_free, p_ar[ppix]
    p_ar[ppix] = ptr_new(arr)
  endfor
  
  return, p_ar
end

;+
; :Description:
;    Aggregate a timeseries for periods of 8, 10, 16 days, month or year. This also depends
;    on the input date interval (the output interval is always larger).<p> 
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
pro nrs_timed_aggregation, image $
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
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef

  ; parameter checking
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif

  nr_levels = 0
  if n_elements(aggr_interval) gt 0 then nr_levels++
  if n_elements(aggr_interval2) gt 0 then nr_levels++
  if nr_levels gt 0 then begin
    all_intervals = ['day', '8-day', '10-day', '16-day', 'month', 'year']
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
        ans = dialog_message('Duplicate period, ignoring', title = 'Warning')
        nr_levels--
      endif else if inter_ix > inter_ix2 then begin
        ; swap periods, so level 1 is the shorter period
        h = inter_ix2
        inter_ix2 = inter_ix
        inter_ix = inter_ix2
        h = aggr_interval
        aggr_interval = aggr_interval2
        aggr_interval2 = h
      endif 
    endif
    
  endif
  
  all_functions = ['mean', 'sum', 'minimum', 'maximum', 'median']
  func_ix = where(strlowcase(aggr_func) eq all_functions, cnt)
  if cnt eq 0 then begin
    ans = dialog_message('Unsupported aggregation function (' + aggr_func + ')', title = 'Error', /error)
    return
  endif
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_taggr', ext = '.')
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Timeseries aggregation'

  per = nrs_get_period_from_range(sd, ed, nb, per_str = input_period)
  
  ; get the indices for the input image stack
  nrs_get_dt_indices, [sd, ed], period = input_period $
                      , julian_out = jul_in, indices = ind_in
                      
  ; get the indices for the output image stack
  ; shortest period first
  nrs_get_dt_indices, jul_in, period = aggr_interval $
                      , julian_out = jul_out $
                      , indices = indices, start_year_index = syi, num_period = num_periods

  ; second period, if any
  nrs_get_dt_indices, jul_in, period = aggr_interval2 $
                      , julian_out = jo_lvl2 $
                      , indices = ind_lvl2, start_year_index = syi_lvl2, num_period = num_lvl2

  ; Always calculate year indices
  nrs_get_dt_indices, jul_in, period = 'year' $
                      , julian_out = jul_yy $
                      , indices = ind_yy

  case nr_levels of
    0 : begin
          nb_out = 1
          bnames = [aggr_func]
        end
    1 : begin
          cr_ix = nrs_get_cross_index(sd, ed, per, input_period, aggr_interval, aggr_interval2)
          caldat, jul_out, mm, dd, yy
          nb_out = min([n_elements(jul_out) - 1, num_periods])
          aggr = fltarr(ns, nb_out)
          
          start = min((num_periods - syi + indgen(nb_out)) mod num_periods)
          bn = reform(1 + (indgen(nb_out) + start) mod num_periods, 1, nb_out)
          bnames = aggr_interval + string(bn, format = '(" ",i02)')
          
        end  
    2 : begin
          ; find boundaries of larger period in shorter periods
          nrs_match_arrays, jul_out, jo_lvl2, index = bounds
          nb_out = n_elements(jul_out) - 1
          aggr = fltarr(ns, nb_out)
          
          start = num_periods - syi
          bn = reform(1 + (indgen(nb_out) + start) mod num_periods, 1, nb_out)
        end
  endcase

  openw, unit, outname, /get_lun

  for l = 0, nl - 1 do begin
    if l mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    case nr_levels of
      0 : begin
            dim = size(data, /n_dim)
            case func_ix of
              0 : aggr = mean(data, dimension = dim)
              1 : aggr = total(data, dim)
              2 : aggr = min(data, dimension = dim)
              3 : aggr = max(data, dimension = dim)
              4 : aggr = median(data, dimension = dim)
            endcase
          end
      1 : begin
            
            for t = 0, n_elements(indices) - 2 do begin
;              for r = 0, n_elements(h_1) - 1 do begin
;                accu = data[*, indices[t] : indices[t + 1] - 1]
                
;              dim = size(accu, /n_dim)
;              case func_ix of
;                0 : aggr[*, t] = mean(accu, dimension = dim)
;                1 : aggr[*, t] = total(accu, dim)
;                2 : aggr[*, t] = min(accu, dimension = dim)
;                3 : aggr[*, t] = max(accu, dimension = dim)
;                4 : aggr[*, t] = median(accu, dimension = dim)
;              endcase
            endfor
          end
      2 : begin
            ; TODO: rearrange indices to align with output
            for t = 0, n_elements(indices) - 2 do begin
              accu = data[*, indices[t] : indices[t + 1] - 1]
              dim = size(accu, /n_dim)
              case func_ix of
                0 : aggr[*, t] = mean(accu, dimension = dim)
                1 : aggr[*, t] = total(accu, dim)
                2 : aggr[*, t] = min(accu, dimension = dim)
                3 : aggr[*, t] = max(accu, dimension = dim)
                4 : aggr[*, t] = median(accu, dimension = dim)
              endcase
            endfor
          end
    endcase
    
    writeu, unit, aggr
  endfor

  ; Now determine band names
  bn = reform(1 + (indgen(nb_out) + syi) mod num_periods, 1, nb_out)
  caldat, jul_out, mm, dd, yy
  yy = reform(yy, 1, nb_out, /overwrite)
  bnames = string([bn, yy], format = '("Period.year ",i02, ".", i04)')

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
  free_lun, unit  ; close assoc
end
