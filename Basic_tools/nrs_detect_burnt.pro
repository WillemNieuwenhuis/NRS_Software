;+
; :description:
;     Determine (suspected) burnt area from timeseries; also determine
;     the length of the time after burning until recovery (exceeding the threshold again)
;
;     Assumption on the input table:
;     - Column 1: date field
;     - Column 2: threshold lower bound
;     - Column 3: threshold upper bound (not used)
;     - Column 4-n: each contains one independent timeseries 
;
;     The output gives two files (CSV formatted)
;     - Output with all columns containing a flag indicating
;         the value is lower than the threshold for each date
;     - Output table with each row containing a record with the
;       observation ID, start date of burn period, end date of burn period, length of period
;
; :params:
;    table : in
;      Input CSV file
;    
; :keywords:
;    basename : in, optional
;      The base name of the output files; if not specified its is calculated from
;      the input file.
;    strict : in, optional, default = no
;      If specified and set causes the function to ignore noise when determining
;      if an area is burnt or not. If not set the function will react to the first exceedance
;      of the threshold.
;    fixed_thresh : in, optional
;      If specified causes the function to use a fixed lower threshold. If not specified the
;      values from the lower threshold column will be used instead.
;    single_thresh : in, optional, default = yes
;      In case fixed_thresh is not specified. If single_thresh is specified and not zero
;      will cause the function to average the threshold column to calculate the lower threshold. 
;    prog_obj : in, optional
;      ProgressBar object for displaying progress
;    cancelled : out, optional
;      If set indicates failure or stopping of the progress by the user
;
; :author: nieuwenhuis
; 
; :history:
;   changes::
;     21 nov 2013: nieuwenhuis, created
;-
pro nrs_detect_burnt, table $
                    , basename = basename $
                    , strict = strict $
                    , fixed_thresh = fixed_thresh $
                    , single_thresh = single_thresh $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  strict = keyword_set(strict)
  useFixed = n_elements(fixed_thresh) gt 0
  useThreshAvg = ~useFixed && keyword_set(single_thresh)
  useFixed = useFixed || useThreshAvg
  
  asc = nrs_read_csv(table, header = header, date_sep = '-/', time_sep = ':')
  field_count = n_tags(asc)
  if field_count lt 4 then begin
    void = error_message('No data fields founds, nothing to do. Quitting...', trace = 0)
    return
  endif
  
  dates = asc.(0)
  nr_dates = n_elements(dates)
  jd = nrs_str2julian(dates)
  thresh_upper = asc.(1)
  thresh_lower = asc.(2)
  if total(thresh_upper - thresh_lower) lt 0 then begin
    temp = temporary(thresh_upper)
    thresh_upper = temporary(thresh_lower)
    thresh_lower = temporary(temp)
  endif
  
  cancelled = 0
  
  if n_elements(basename) eq 0 then basename = getOutname(table, postfix = '_burn', ext = '.csv')
  burn_filename = getOutname(basename, postfix = '_flag', ext = '.csv')
  period_filename = getOutname(basename, postfix = '_per', ext = '.csv')
  
  openw, unit_p, period_filename, /get_lun
  printf, unit_p, header[0] + ',start,end,#periods,start_date,end_date,#days'
  
  data_out = make_array(field_count - 3, nr_dates, type = 1)
  
  if useThreshAvg then fixed_thresh = mean(thresh_lower)

  for loc = 0, field_count - 4 do begin
    id = header[loc + 3]
    ts = asc.(loc + 3)
    
    detect = useFixed ? (ts lt fixed_thresh) : (ts lt thresh_lower)
    if ~strict then begin 
      cand = detect
      for i = 1, n_elements(cand) - 2 do begin
        detect[i] = (cand[i - 1] * (1 - cand[i]) * cand[i + 1]) gt 0 ? 1 : cand[i]
      endfor
    endif
    
    data_out[loc, *] = detect
    consec, detect, low, high, n, /same
    dst = []
    start = []
    eind = []
    dst = (high - low + 1) * (detect[low] gt 0)
    ix = where(dst gt 0, cnt)
    if cnt gt 0 then begin
      dst = dst[ix]
      start = low[ix]
      eind = high[ix]
      sd = dates[start]
      ed = dates[eind]
      nd = jd[eind] - jd[start]
      for r = 0, cnt - 1 do begin
        printf, unit_p, strjoin([id, string([start[r] + 1, eind[r] + 1, dst[r]], form = '(i0)') $
                              , sd[r], ed[r], string(nd[r], form = '(i0)')], ',')
      endfor
    endif

  endfor
  close, unit_p
  free_lun, unit_p
  
  openw, unit_b, burn_filename, /get_lun
  printf, unit_b, strjoin(header[[0, indgen(field_count - 3) + 3]], ',')
  for r = 0, nr_dates - 1 do begin
    printf, unit_b, strjoin([dates[r], string(data_out[*, r], form = '(i0)')], ',')
  endfor
  close, unit_b
  free_lun, unit_b
end
