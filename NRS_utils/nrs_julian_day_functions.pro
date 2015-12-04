;+
; :Description:
;    Find the most probable mapping for day, month and year.
;
; :Params:
;    dt_arr : in
;      In columns 0..2 contains the values for days, month and year, but the order
;      is uncertain.
;
; :Returns:
;   A calculated guess to which columns are day, month and year
;
; :Author: nieuwenhuis
;-
function nrs_dmy_mapping, dt_arr
  if size(dt_arr, /n_dim) le 1 then mx = dt_arr $
  else mx = max(dt_arr, dimension = 2)
  dmy = [0, 1, 2] ; default order = d-m-y
  if mx[0] gt 31 then begin
    dmy = [2, 1, 0]
    if mx[1] gt 12 then dmy = [1, 2, 0]
  endif else if mx[2] gt 31 then begin
    dmy = [0, 1, 2]
    if mx[1] gt 12 then dmy = [1, 0, 2]
  endif else begin
    dmy = [0, 1, 2]
    if mx[1] le 12 then begin
      if mx[2] le 12 then dmy = [0, 1, 2] $
      else dmy = [2, 0, 1]
    endif else begin
      dmy = [2, 0, 1]
      if mx[0] le 12 then dmy = [1, 0, 2]
    endelse
  endelse
  
  return, dmy
end

;+
; :Description:
;    Split the string formatted date and time (arrays) into separate fields for
;    all date and time components. Date components are assumed to be separated by
;    either '-' or '/'. Time components are assumed to be separated by ':'.
;    The time zone is not considered. 
;
; :Params:
;    date : in
;      Date as string (date components in numerical format, f.e.: 24-11-1999)
;    time : in
;      Time as string
;
; :Returns:
;   An array with six columns, where columns 0..2 contain the date components
;   and the columns 3:5 contain the time components (hour, minute, seconds)
;
; :Author: nieuwenhuis
;-
function nrs_split_datetime, date, time
  compile_opt idl2
  dt_arr = intarr(6)
  parts = strsplit(date, '-/', /extract, count = nr_dt)
  dt_arr[0 : nr_dt - 1] = fix(parts)
  if n_elements(time) gt 0 then begin
    if strlen(time) gt 0 then begin 
      parts = strsplit(time, ':', /extract, count = nr_tm)
      dt_arr[3 : 3 + nr_tm - 1] = fix(parts)
    endif
  endif
  
  return, dt_arr 
end

;+
; :Description:
;    Convert date-time strings to a julian date. The date components
;    are assumed to be numerical. The input can either be a single string or
;    a string array.
;    
;    The date string can contain a date or a date and time
;
; :Params:
;    datetime : in
;      String data with date and time
;
; :Returns:
;   Julian date, or 0 if conversion failed
;
; :Author: nieuwenhuis
; 
; :history:
;   changes::
;     14 nov 2013: nieuwenhuis, now accepts either a string or a string array
;     17 jan 2012: nieuwenhuis, date only dates return long values; date and time
;                               return floating points values
;     26 dec 2011: nieuwenhuis, created
;-
function nrs_str2julian, datetime
  compile_opt idl2

  date_count = n_elements(datetime)
  out = []
  for s = 0, date_count - 1 do begin
    parts = strsplit(datetime[s], ' ' + string(9b), /extract, count = nr_dt)
    if nr_dt eq 0 then continue
    
    if nr_dt eq 1 then parts = [parts, '']
    dt_arr = nrs_split_datetime(parts[0], parts[1])
    dmy = nrs_dmy_mapping(dt_arr)
    
    if dt_arr[dmy[2]] lt 100 then continue
    
    if nr_dt eq 1 then begin
      out = [out, julday(dt_arr[dmy[1]], dt_arr[dmy[0]], dt_arr[dmy[2]])]
    endif else begin
      out = [out, julday(dt_arr[dmy[1]], dt_arr[dmy[0]], dt_arr[dmy[2]], dt_arr[3], dt_arr[4], dt_arr[5])]
    endelse
  endfor
  
  if n_elements(out) eq 0 then return, out
  
  return, date_count gt 1 ? out : out[0]
end

function nrs_dmy_from_julian, julian
  compile_opt idl2
  
  caldat, julian, mm, dd, yy
  
  dmy = yy * 100000 + mm * 100 + dd

  return, dmy
end

function nrs_julian_from_dmy, dmy
  dm = dmy mod 10000
  y = dmy / 10000
  m = dm / 100
  d = dm mod 100
  if y gt 0 then $
    return, julday(m, d, y) $
  else $
    return, julday(m, d, 1970) - julday(1, 1, 1970)
end

;+
; :Description:
;    Calculate the doy from a julian date. Optionally also the year is calculated
;
; :Params:
;    julian : in, required
;      A single julian date, or an array with julian dates
;
; :Keywords:
;    year : out
;      If specified, contains the year(s) of each of the input julian dates.
;
; :Author: nieuwenhuis
;-
function nrs_doy_from_julian, julian, year = year
  caldat, julian, m, d, year
  d1 = d - d + 1
  m1 = m - m + 1
  jan1 = julday(m1, d1, year)
  return, julian - jan1 + 1
end

function nrs_julian_from_doy, doy, year
  jan1 = julday(1, 1, year)
  return, jan1 + doy - 1
end

function nrs_julian_as_string, julian, time = time, format = dtform
  compile_opt idl2

  formats_dt = [ '(c(cyi4.4,"-",cmoi2.2,"-",cdi2.2," ",cHi2.2,":",cmi2.2,":",csi2.2))' $
               , '(c(cdi2.2,"-",cmoi2.2,"-",cyi4.4," ",cHi2.2,":",cmi2.2,":",csi2.2))']
  formats_d =  [ '(c(cyi4.4,"-",cmoi2.2,"-",cdi2.2))' $
               , '(c(cdi2.2,"-",cmoi2.2,"-",cyi4.4))']

  if n_elements(dtform) eq 1 then dtform = (dtform > 0) < 1 $
  else dtform = 0
  
  if keyword_set(time) then begin
    return, string(julian, format = formats_dt[dtform])
  endif else begin
    return, string(julian, format = formats_d[dtform])
  endelse 
end

function nrs_dmy_as_string, dmy
  dm = dmy mod 10000
  y = dmy / 10000
  m = dm / 100
  d = dm mod 100
  if y gt 0 then $
    return, string(y, format = '(i04)') + '-' + string(m, format = '(i02)') + '-' + string(d, format = '(i02)') $
  else $
    return, string(m, format = '(i02)') + '-' + string(d, format = '(i02)')
end