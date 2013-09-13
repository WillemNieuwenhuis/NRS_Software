;+
; :description:
;    Calculate the annual total precipitation for wet days (>1.0 mm)
;
; :params:
;    inname : in, required
;      Name of the input time series
;;    startday : in
;      The start date (as julian day, not doy!)
;    endday : in
;      The end date (as julian day)

; :keywords:
;    outname : in
;      The name of the output
;    limit : in, optional, default = 1 mm
;      The limit dividing dry and wet
;    prog_obj : in, optional
;      Progressbar object to indicate progress of the calculation
;    cancelled : in, optional
;      If a progressbar is used, signals that the user stopped the calculation
;
; :author: nieuwenhuis
;-
pro nrs_climind_prcptot, inname  $
           , outname = outname $
           , startday, endday $
           , limit = limit $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  if n_elements(limit) gt 0 then limit = float(limit) else limit = 1.0
  
  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  if ~((n_elements(undef) gt 0) && (undef ne 1e34)) then undef = -9999
  
  nrs_set_progress_property, prog_obj, /start, title = 'Precipitation indices'

  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then begin
    outname = getOutname(inname, postfix = '_ptot', ext = '.dat')
  endif

  count = 0
  caldat, [startday, endday], mm, dd, yy
  sy = yy[0]
  ey = yy[1]
  nr_years = ey - sy + 1 
  nrs_get_dt_indices, [startday, endday + 1], period = 'year', julian_out = jm
  
  if nb ne (endday - startday + 1) then begin
    void = error_message('Probably not daily data, quitting')
    return
  endif

  cancelled = 0
    
  openw, unit, outname, /get_lun
  
  bn = indgen(nr_years) + sy
  bnames = [string(bn, format = '(i0)')]
    
  out_data = fltarr(ns, nr_years)

  jm -= startday
  pos = indgen(nb)
  sl_tot = ns * nb
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    ix = where(slice lt limit, cnt)
    
    if cnt eq sl_tot then out_data[*] = 0.0 $
    else begin
      slice[ix] = 0
      for y = 0, nr_years - 1 do begin
        ps = jm[y]
        pe = jm[y + 1] - 1
        out_data[*, y] = total(slice[*, ps : pe], 2) 
      endfor
    endelse
    
    writeu, unit, out_data
  endfor
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  dt = size(out_data, /type)
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nr_years, nl = nl, ns = ns $
          , bnames = bnames $
          , inherit = meta $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close output file
end

