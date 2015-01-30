pro nrs_timesat_EstimatedTime_strings, t1, t2, nelem = nelem, s_persec, s_estim, is_end
  compile_opt idl2, logical_predicate

  timediff = t2 - t1
  estimatedHours = timediff
  nitems = 1
  if n_elements(nelem) eq 1 then nitems = nelem
  estimatedHours = timediff * nitems / 3600
    
  hour = floor(estimatedHours)
  mins = (estimatedHours - hour) * 60
  minutes = floor(mins)
  secs = (mins - minutes) * 60
  s_persec = 'Elapsed time by cleaning one pixel: ' + string(timediff, format='(i0)') + ' seconds'
  if is_end eq 0 then begin
    s_estim = 'Estimated running time: ' $
        + string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)') $
        + ' hours to finish'
  endif else begin
    s_estim = 'Actual running time: ' $
        + string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)') $
        + ' hours.'
  endelse
end

function nrs_time_to_string, seconds
  compile_opt idl2, logical_predicate

  hours = (seconds / 3600) mod 24 ; only interested in day time
  hour = floor(hours)
  mins = (hours - hour) * 60
  minutes = floor(mins)
  secs = (mins - minutes) * 60
  return, string(hour, format='(i0)') + ':' + string(minutes, format='(i02)') + ':' + string(secs, format='(i02)')
end

pro nrs_timesat_printEstimatedTime, t1, t2, nelem = nelem
  compile_opt idl2, logical_predicate

  nrs_timesat_EstimatedTime_strings, t1, t2, nelem = nelem, s_persec, s_estim

  print, s_persec
  print, s_estim
end

