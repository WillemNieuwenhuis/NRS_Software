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
  if strlen(time) gt 0 then begin 
    parts = strsplit(time, ':', /extract, count = nr_tm)
    dt_arr[3 : 3 + nr_tm - 1] = fix(parts)
  endif
  
  return, dt_arr 
end

;+
; :Description:
;    Convert a date-time string to a julian date. The date components
;    are assumed to be numerical 
;
; :Params:
;    datetime : in
;      String with date and time
;
; :Returns:
;   Julian date, or 0 if conversion failed
;
; :Author: nieuwenhuis
;-
function nrs_str2julian, datetime
  compile_opt idl2
  parts = strsplit(datetime, ' ' + string(9b), /extract, count = nr_dt)
  if nr_dt eq 0 then return, 0
  
  if nr_dt eq 1 then parts = [parts, '']
  dt_arr = nrs_split_datetime(parts[0], parts[1])
  dmy = nrs_dmy_mapping(dt_arr)
  
  if dt_arr[dmy[2]] lt 100 then return, 0
  
  if nr_dt eq 1 then return, julday(dt_arr[dmy[1]], dt_arr[dmy[0]], dt_arr[dmy[2]])
  
  return, julday(dt_arr[dmy[1]], dt_arr[dmy[0]], dt_arr[dmy[2]], dt_arr[3], dt_arr[4], dt_arr[5] )
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

function nrs_julian_as_string, julian
  caldat, julian, mm, dd, year
  
  if max(year) lt 100 then $
    return, string(mm, format = '(i02)') + '-' + string(dd, format = '(i02)') $
  else $ 
    return, string(year, format = '(i04)') + '-' + string(mm, format = '(i02)') + '-' + string(dd, format = '(i02)') 
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