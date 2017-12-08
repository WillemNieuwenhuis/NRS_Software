;+
; :Description:
;   Read a climdex organised data file. Per line it contains: date, daily precipitation, optionally daily min/max temperature: 
;   Data per line: yyyy mm dd   prec  tmax  tmin
;     or           yyyy mm dd   prec
;   The delimiters accepted are ' ' (space), '-' or ','
;  
;   The file contains daily observations, but can have missing dates.
;   Missing observations are indicated by the value: -99.9 
;
;   The output contains the checked values for all the dates within the range, including the missing dates
;   The range is extended to include the full years at start and end of the range.
;   
;   Simple checking is already done:
;   - check on valid range: if outside set to NaN
;   - missing values are set to NaN
;   - if min temperature is higher than max temperature on a particular date, both are set to NaN
;
; :Returns:
;   0 if all data ar missing, 1 otherwise 
;
; :Params:
;    datafile : in
;      The (text) datafile
;
; :Keywords:
;    data : out
;      Returns a struct when data could be loaded successfully. The struct contains fields 
;      Precip, StartYear, EndYear, Dates (as julian date), and optionally TMax, TMin
;        precip: Daily precipitation values. Valid range is [0, ...>
;        start_year: The year of the first observation
;        end_year:The year of the last observation
;        julian: Julian dates of all observations (includes 29 feb)
;        tmax: Daily temperatures (max each day). Valid range is [-70, 70]
;        tmin: Daily temperatures (minimum each day). Valid range is [-70, 70]
;    na_stats : out, optional
;      If specified, performs an additional data checks: check whether ther are multiple missing values
;      either per month (max 3 missing) or per year (max 15 missing). The na_stats will be populated with 
;      two arrays: one with month/year data (12 x nr_years) and one with year data (nr_years)
;      Each array contains 3 columns indicating missing data in: tmax, tmin, precipation (in that order)
;
; :Author: nieuwenhuis
; 
; :History:
;   - november 2017: created
;-
function nrs_climdex_load, datafile, data = data_collect, na_stats = na_stats
  compile_opt idl2, logical_predicate

  data_collect = []
  
  fi = file_info(datafile)
  if ~fi.exists then begin
    ;    write_log, ';QC; File does not exist: ' + datafile
    return, 0
  endif

  openr, unit, datafile, /get_lun, error = err
  if err ne 0 then begin
    ;    write_log, 'Error opening file: ' + datafile
    return, 0
  endif

  missing = -!values.f_nan

  nr_lines = file_lines(datafile)
  lst = strarr(nr_lines)
  readf, unit, lst
  close, unit
  free_lun, unit
  line = strtrim(lst[0],2)
  flds = strsplit(line, ' ,', /extract)
  nr_fields = n_elements(flds)
  do_temp = nr_fields ge 5
  
  filedata = fltarr(nr_fields, nr_lines)
  lpos = 0
  for line = 0, nr_lines - 1 do begin
    ldat = strtrim(lst[line], 2)
    if strlen(ldat) eq 0 then continue  ; skip empty lines
    
    record = float(strsplit(ldat, ' ,', /extract))
    jd = julday(record[1], record[2], record[0])
    caldat, jd, m, d, y
    if m ne record[1] then continue   ; skip invalid dates (30 feb, 31 april, etc)
    
    filedata[*, lpos] = record
    lpos++
  endfor
  filedata = filedata[*, lindgen(lpos)]
  ix = where(filedata lt -90.0, cnt)
  if cnt ne 0 then filedata[ix] = missing

  start_year = filedata[0, 0]
  end_year = filedata[0, -1]
  nr_years = end_year - start_year + 1
  nrs_get_dt_indices, [julday(1, 1, start_year), julday(12, 31, end_year)], period = 'day', julian_out = julian
  ; juliand now contains the complete set of dates in the years from start to end as julian date
  total_days = n_elements(julian)

  jdix = julday(filedata[1, *], filedata[2, *], filedata[0, *]) ; all julian dates in the input data

  ; put data in precip, tmin and tmax arrays
  data_ix = jdix - jdix[0]  ; turn dates into indices
  
  precip = fltarr(total_days) + missing
  precip[data_ix] = filedata[3, *]
  pix = where(precip lt 0, p_cnt)
  if p_cnt gt 0 then precip[pix] = missing
  p_miss = total(precip eq missing)

  temp_miss = 0
  if do_temp then begin
    tmin = fltarr(total_days) + missing
    tmin[data_ix] = filedata[5, *]
    tmax = fltarr(total_days) + missing
    tmax[data_ix] = filedata[4, *]

    ; check on tmin > tmax
    tix = where(tmin gt tmax, tcnt)
    if tcnt gt 0 then begin
      tmax[tix] = missing
      tmin[tix] = missing
    endif

    ; check if values are in range
    outrange = (tmax lt -70) * (tmax gt 70)
    thix = where(outrange eq 1, th_cnt)
    if th_cnt gt 0 then tmax[this] = missing
    outrange = (tmin lt -70) * (tmin gt 70)
    tlix = where(outrange eq 1, tl_cnt)
    if tl_cnt gt 0 then tmin[this] = missing

    th_miss = total(tmax eq missing)
    tl_miss = total(tmin eq missing)
    temp_miss = (tl_miss eq total_days) && (tl_miss eq total_days) 
  endif
  
  ; check for all data is missing
  if temp_miss && (p_miss eq total_days) then begin
    return, 0
  endif
  
  if arg_present(na_stats) then begin
    ; check for multiple missing per month / year
    caldat, julian, mm, dd, yy
    ; per year / month
    ym = (yy - yy.min())* 12 + mm - 1
    hym = histogram(ym, rev = ri, min = 0, nbins = ym.max() + 1)
    mon_miss = bytarr(ym.max() + 1, 3)
    for i = 0, n_elements(hym) - 1 do begin
      if do_temp then begin
        mon_miss[i, 0] = ( total(~finite(tmax[ri[ri[i]:ri[i+1]-1]])) ) gt 3 ? 1 : 0
        mon_miss[i, 1] = ( total(~finite(tmin[ri[ri[i]:ri[i+1]-1]])) ) gt 3 ? 1 : 0
      endif
      mon_miss[i, 2] = ( total(~finite(precip[ri[ri[i]:ri[i+1]-1]])) ) gt 3 ? 1 : 0
    endfor
    
    ; per year
    yyh = yy - yy.min()
    hy = histogram(yyh, rev = ri, min = 0, nbins = yyh.max() + 1)
    year_miss = bytarr(yyh.max() + 1, 3)
    for i = 0, n_elements(hy) - 1 do begin
      if do_temp then begin
        year_miss[i, 0] = ( total(~finite(tmax[ri[ri[i]:ri[i+1]-1]])) ) gt 15 ? 1 : 0
        year_miss[i, 1] = ( total(~finite(tmin[ri[ri[i]:ri[i+1]-1]])) ) gt 15 ? 1 : 0
      endif
      year_miss[i, 2] = ( total(~finite(precip[ri[ri[i]:ri[i+1]-1]])) ) gt 15 ? 1 : 0
    endfor
    na_stats = create_struct('na_month', mon_miss, 'na_year', year_miss)
  endif
  
  ; create nameless struct; named struct might get into conflict with other dataset with different number of years
  if do_temp then begin
    data_collect = create_struct( $
                                   'TMax', tmax, 'TMin', tmin, 'Precip', precip $
                                 , 'Dates', julian $
                                 , 'StartYear', start_year, 'EndYear', end_year)
  endif else begin
    data_collect = create_struct( $
                                   'Precip', precip $
                                 , 'Dates', julian $
                                 , 'StartYear', start_year, 'EndYear', end_year)
  endelse
  
  return, 1
end

pro nrs_climdex_qc, mmp
  compile_opt idl2, logical_predicate

  ; check for all data missing
  if valid eq 0 then return
  
  start_year = mmp.startyear
  end_year = mmp.endyear
  jul_obs = mmp.dates

  nr_years = end_year - start_year + 1
  
  ; temperature: daily mean and stddev, both of tmin and tmax, over the entire period
  ; missing values are handled
  ; 29 feb is not considered
  caldat, jul_obs, m, d, y
  daymon = m * 100 + d
  iall = where(daymon ne 229)  ; select all dates except 29 feb
  m_reg = m[iall]
  d_reg = d[iall]
  y_reg = y[iall]                     ; all dates in the input data except 29 feb
  daymon = daymon[iall]
  tmax = (mmp.tmax)[iall]
  tmin = (mmp.tmin)[iall]

  T_stats = fltarr(365, 4)   ; values are: Tmin[mean, std], tmax[mean, std]
  for dm = 0, 365 - 1 do begin
    iyx = where(daymon eq daymon[dm], cnt)  ; select a particular day for all years
    mx = tmax[iyx]
    mn = tmin[iyx]
    void = moment(mn, mean = tmin_mean, sdev = tmin_std, /nan)
    void = moment(mx, mean = tmax_mean, sdev = tmax_std, /nan)
    T_stats[dm, *] = [tmin_mean, tmin_std, tmax_mean, tmax_std] 
  endfor

;  nrs_log_line, logfile, line, append = append, header = header
    
;  openw, plog, plogfile, /get_lun
;  openw, tlog, tlogfile, /get_lun
  
  
end

pro nrs_climdex, datafile
  compile_opt idl2, logical_predicate
  
  valid = climdex_load(datafile, data = mmp)

  if valid then begin
    nrs_climdex_qc, mmp
  endif
end
