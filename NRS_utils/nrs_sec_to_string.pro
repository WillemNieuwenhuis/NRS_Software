;+
; :description:
;    Convert a time in seconds to a date-time string with
;    days (optional) and hours, minutes, seconds
;
; :params:
;    seconds
;
; :keywords:
;    time : in, optional, default = 1
;      if set return the string with time (hours, minutes, seconds)
;    date : in, optional, default = 0
;      if set return the string with date (in days, if larger than zero)
;    hours24 : in, optional, default = 0
;      if set will display hours also if larger than 24, but only if date keyword is not set 
;
; :author: nieuwenhuis
;-
function nrs_sec_to_string, seconds, time = time, date = date, hours24 = hours24
  compile_opt idl2
  
  time = keyword_set(time)
  date = keyword_set(date)
  time = time || ~date
  
  do24h = keyword_set(hours24) && ~date
  
  days = fix(seconds / (3600L * 24))
  hours = (seconds / 3600) mod 24
  hour = floor(hours)
  mins = (hours - hour) * 60
  minutes = floor(mins)
  secs = (mins - minutes) * 60
  sec10 = fix(secs / 10) 
  secmil = secs - sec10 * 10
  
  outstr = ''
  if date && (days gt 0) then $
    outstr += string(days, format = '(i0,"d ")')
  if time then begin
    if do24h && (days gt 0) then outstr += string(days * 24 + hour, format = '(i0)') + ':' $
    else outstr += string(hour, format='(i0)') + ':'
    outstr += string(minutes, format='(i02)') + ':' $
            + string(sec10, format='(i0)') $
            + string(secmil, format='(f0.3)')
  endif

  return, outstr
end

