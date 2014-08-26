pro _auto_build, files, sav_name, logfile = logfile, no_lib = no_lib
  routines = nrs_find_my_routines(/exclude_sav, files = files, no_lib = no_lib)
  rout_cnt = n_elements(routines.name)
  
  urout = []
  usrc = []
  if rout_cnt le 0 then msg = sav_name + ' not built, no modules' $
  else begin
    six = sort(routines.name)
    sorted = routines.name[six]
    sort_src = routines.source[six]
    uix = uniq(sorted)
    urout = sorted[uix]
    usrc = sort_src[uix]
    sav_cmd = 'save, filename = "' + sav_name + '", /routines'
    sav_cmd += ',' + strjoin(string(urout, format = '("''",a,"''")'), ',')

    if execute(sav_cmd) then msg = sav_name + ' successfully created' $
    else msg = sav_name + ' build failed'
  endelse
  print, msg
  
  if n_elements(logfile) gt 0 && strlen(strtrim(logfile, 2)) gt 0 then begin
    openw, unit, logfile, /get_lun, /append
    printf, unit, systime()
    if rout_cnt eq 1 then $
      printf, unit , urout, usrc, for = '(a,",",a)
    if rout_cnt gt 1 then $
      printf, unit ,[transpose(urout),transpose(usrc)], for = '(a,",",a)
    printf, unit
    printf, unit, sav_cmd
    printf, unit
    printf, unit, msg
    printf, unit, '-------------'
    close, unit
  endif
  
end

pro _auto_build_version, name = name
  compile_opt idl2
  
  format = '(c(cyi4.4,"-",cmoi2.2,"-",cdi2.2," ",cHi2.2,":",cmi2.2,":",csi2.2))'
  
  openw, unit, name + '_version.pro', /get_lun
  printf, unit, 'pro ' + name + '_version'
  printf, unit, '  print, "' + name + ' build date: ' + string(systime(/julian), format = format) + '"'
  printf, unit, 'end'
  close, unit
  free_lun, unit
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