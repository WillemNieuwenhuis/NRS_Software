pro nrs_precind_rx5day, image, startday, endday, outname = outname $
                      , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, data_ignore_value = undef
  inherit = envi_set_inheritance(fid, dims, /full)
  
  missing = -9999.
  if ~((n_elements(undef) gt 0) && (undef ne 1e34)) then undef = missing 
  count = 0
  caldat, [startday, endday], mm, dd, yy
  sy = yy[0]
  ey = yy[1]
  
  if nb ne (endday - startday + 1) then return
  
  cancelled = 0

  nrs_set_progress_property, prog_obj, /start, title = 'Calculate RX5day / RX1day precipitation indices'

  if n_elements(outname) eq 0 then begin
    outname_rx5 = getOutname(image, postfix = '_rx5', ext = '.dat')
    outname_rx1 = getOutname(image, postfix = '_rx1', ext = '.dat')
  endif else begin
    outname_rx5 = getOutname(outname, postfix = '_rx5', ext = '.dat')
    outname_rx1 = getOutname(outname, postfix = '_rx1', ext = '.dat')
  endelse
    
  openw, unit5, outname_rx5, /get_lun
  openw, unit1, outname_rx1, /get_lun
    
  ; setup index arrays
  pos = indgen(nb)
  ix = transpose(reform(rebin(indgen(5), 5 * 27), 27, 5))
  ix2 = reform(rebin(indgen(27), 5 * 27), 5, 27)
  ix5 = ix + ix2

  ixs = indgen(31)
  nrs_get_dt_indices, [startday, endday + 1], period = 'month', julian_out = jm
  dpm = [0, total(jm[1:*] - jm[0:-2], /cumulative, /integer)]
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit1
      close, unit5
      free_lun, unit1
      free_lun, unit5
      return
    endif
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
    ixundef = where(data eq undef, cnt_undef)
    data[ixundef] = 0 ; set all undef values to zero
  
    rx5day = fltarr(ns, n_elements(dpm) - 1)
    rx1day = fltarr(ns, n_elements(dpm) - 1)
    for m = 0, n_elements(dpm) - 2 do begin
      nrdays = dpm[m + 1] - dpm[m]
      low_ix = dpm[m]
      high_ix = dpm[m + 1]
      datx = reform(data[*, low_ix + ix5[*, 0 : nrdays - 5]], ns, 5, nrdays - 4)
      r5 = total(datx, 2)
      rx5day[*, m] = max(r5, dim = 2)
      
      rx1day[*, m] = max(data[*, low_ix + ixs[0 : nrdays - 1]], dim = 2)  
    endfor
    
    writeu, unit5, rx5day
    writeu, unit1, rx1day
  endfor

  caldat, jm[0 : -2], mm, dd, yy
  bnames = string([transpose(yy), transpose(mm)], format = '(i4,i02)')
  
  envi_setup_head, fname = outname_rx1 $
        , ns = ns, nl = nl, nb = n_elements(dpm) - 1 $
        , data_type = 4 $   ; 4 = float
        , bnames = bnames $
        , /write $
        , interleave = 1 $  ; 1 == BIL
        , inherit = inherit
  
  envi_setup_head, fname = outname_rx5 $
        , ns = ns, nl = nl, nb = n_elements(dpm) - 1 $
        , data_type = 4 $   ; 4 = float
        , bnames = bnames $
        , interleave = 1 $  ; 1 == BIL
        , /write $
        , inherit = inherit
  
  close, unit1
  close, unit5
  
  free_lun, unit1
  free_lun, unit5
end
