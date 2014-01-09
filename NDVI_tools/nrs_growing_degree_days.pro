pro nrs_growing_degree_days, image, outname = outname $
                  , calc_jerk = calc_jerk $
                  , calc_spring_start = calc_spring_start $
                  , tbase_intercept = tbase_intercept $
                  , tbase_slope = tbase_slope $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  calc_jerk = keyword_set(calc_jerk)
  calc_spring_start = keyword_set(calc_spring_start)
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , fname = image_name $
                 , bnames = bnames $
                 , data_ignore_value = nodata
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then begin
    ans = error_message('Dataset has no coordinates', traceback = 0)
  endif
  
  projection = envi_get_projection(fid = fid)
  geographic = envi_proj_create(/geographic)
  applyProj = (projection.name ne 'Arbitrary') 
  
  ; if not specified use tbase calculation based optimized for latitude between 52N to 72N
  if n_elements(tbase_slope) eq 0 then tbase_slope = -0.25D
  if n_elements(tbase_intercept) eq 0 then tbase_intercept = 13.0D
  
  cancelled = 0
  
  nrs_set_progress_property, prog_obj, /start, title = 'Growing degree days'

  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_gdd', ext = '.dat')
  if calc_jerk then jerk_name = getOutname(outname, postfix = '_jerk', ext = '.dat')
  if calc_spring_start then spring_name = getOutname(outname, postfix = '_spst', ext = '.dat')
  
  openw, unit, outname, /get_lun
  if calc_jerk then openw, unit_jerk, jerk_name, /get_lun
  
  cols = lindgen(ns)
  pos = lindgen(nb)
  ixy = rebin(cols, ns, nb)
  
  gdd_spring = make_array(ns, nl, calc_jerk ? 2 : 1, type = dt)
  gdd_jerk = make_array(ns, nb, type = dt)  ; prepare for BIL
  t_axis = lindgen(nb) 
  
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
      if calc_jerk then begin
        close, unit_jerk
        free_lun, unit_jerk
      endif
      return
    endif
  
    envi_convert_file_coordinates, fid, cols, l, map_x, map_y, /to_map
    if applyProj then begin
      envi_convert_projection_coordinates, map_x, map_y, projection, crd_x, crd_y, geographic
      lat = crd_y
      lon = crd_x
    endif else begin
      lat = map_y
      lon = map_x
    endelse
    tbase = tbase_slope * lat + tbase_intercept
    
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
    gdu = (data - tbase[ixy]) > 0
    
    gdd = total(gdu, 2, /cumulative, /double)
    
    ; calculate start of spring from GDD > 180 degrees centrigade
    if calc_spring_start then begin
      gdd180 = gdd gt 180
      if total(gdd180) gt 0 then begin
        gdd_sh = shift(gdd180, 0, -1)
        diff = gdd_sh - gdd180
        ind = where(diff eq 1)
        sp_ix = array_indices(gdd180, ind)
        gdd_spring[sp_ix[0, *], l, 0] = sp_ix[1, *] + 1
      endif
    endif
    
    if calc_jerk then begin
      ; calculate GDD-jerk
      high = max(gdd, dim = 2)
      for c = 0, ns - 1 do begin
        gdd_loc = gdd[c, *] / high[c] ; normalize
        coef = comfit(t_axis, gdd_loc, [1,1,1], /logistic $
                    , yfit = gdd_fit, itmax = 100, status = status) ; sigmoid fit
        if status gt 0 then begin
          print, 'Fail: max gdd = ' + string(max(gdd_loc)*high[c])
        endif
        gdd1 = deriv(gdd_fit * high[c]) ; use the denormalized fitted curve
        gdd2 = deriv(gdd1)
        gdd3 = deriv(gdd2)
        gdd_jerk[c, *] = gdd3
        if calc_spring_start then begin
          p1 = where(gdd3 le 0, cnt)
          if cnt gt 0 then begin
            mx = max(gdd3[0 : p1[0]], mxpos)
            gdd_spring[c, l, 1] = mxpos[0]
          endif
        endif
      endfor
      writeu, unit_jerk, gdd_jerk
      
;      ; calculate start of spring from gdd jerk
;      mx = max(gdd_jerk, mx_pos, dim = 2)   ; mx_pos is linearized
;      ind = array_indices(gdd_jerk, mx_pos) ; get the rows and columns again
;      gdd_spring[ind[0, *], l, 1] = ind[1, *] + 1     ; use the row as index (time axis)
    endif
    
    writeu, unit, gdd
  endfor
  if calc_spring_start then begin
    spring_bnames = ['GDD > 180']
    if calc_jerk then spring_bnames = [spring_bnames, 'GDD jerk max']
    envi_write_envi_file, gdd_spring, out_name = spring_name $
                        , map_info = mi $
                        , bnames = spring_bnames $
                        , interleave = 0 $ 
                        , /no_realize, /no_open
  endif

  meta = envi_set_inheritance(fid, dims, /full)
  
  close, unit
  free_lun, unit
  envi_setup_head, fname = outname $
          , data_type = size(gdd, /type) $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nb, nl = nl, ns = ns $
          , bnames = bnames $
          , inherit = meta

  if calc_jerk then begin
    close, unit_jerk
    free_lun, unit_jerk
    envi_setup_head, fname = jerk_name $
            , data_type = size(gdd_jerk, /type) $
            , /write $
            , interleave = 1 $  ; BIL
            , nb = nb, nl = nl, ns = ns $
            , bnames = bnames $
            , inherit = meta
  endif

end
