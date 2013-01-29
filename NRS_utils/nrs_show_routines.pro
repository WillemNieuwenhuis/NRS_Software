function nrs_find_sav_routines, savefile, csv_file = csv_file
  sav_obj = obj_new('IDL_Savefile',savefile)
  proc = sav_obj->Names(/proc)
  func = sav_obj->Names(/func)
  
  if n_elements(csv_file) gt 0 then begin
    openw, lun, csv_file, /GET_LUN
    printf, lun, 'Type,name'
    printf, lun, proc, format = '("procedure,",a)'
    printf, lun, func, format = '("function,",a)'

    close, lun
    free_lun, lun
  endif
  return, [proc, func]
end