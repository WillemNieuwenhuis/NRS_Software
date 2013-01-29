pro _auto_build, files, sav_name, logfile = logfile, no_lib = no_lib
  routines = nrs_find_my_routines(/exclude_sav, files = files, no_lib = no_lib)
  rout_cnt = n_elements(routines.name)
  
  if rout_cnt le 0 then msg = sav_name + ' not built, no modules' $
  else begin
    six = sort(routines.name)
    sorted = routines.name[six]
    urout = sorted[uniq(sorted)]
    sav_cmd = 'save, filename = "' + sav_name + '", /routines'
    sav_cmd += ',' + strjoin(string(urout, format = '("''",a,"''")'), ',')

    if execute(sav_cmd) then msg = sav_name + ' successfully created' $
    else msg = sav_name + ' build failed'
  endelse
  print, msg
  
  if n_elements(logfile) gt 0 && strlen(strtrim(logfile, 2)) gt 0 then begin
    openw, unit, logfile, /get_lun, /append
    printf, unit, systime()
    if rout_cnt gt 0 then $
      printf, unit ,[transpose(routines.name),transpose(routines.source)], for = '(a,",",a)
    printf, unit
    printf, unit, sav_cmd
    printf, unit
    printf, unit, msg
    printf, unit, '-------------'
    close, unit
  endif
  
end

pro _manual_build, sav_name
  routines = nrs_find_my_routines(/exclude_sav)
  sav_cmd = 'save, filename = "' + sav_name + '", /routines'
  ix = where(strpos(routines.source, '.pro') eq strlen(routines.source) - 4, rout_cnt)
  if rout_cnt gt 0 then sav_cmd += ',' + strjoin(string(routines.name[ix], format = '("''",a,"''")'), ',') 
  
  if rout_cnt le 0 then print, sav_name + ' not built, no modules' $
  else if execute(sav_cmd) then print, sav_name + ' successfully created (' $
                                       + string(rout_cnt, format = '(i0)') $
                                       + ' modules)' $
  else print, sav_name + ' build failed'
end