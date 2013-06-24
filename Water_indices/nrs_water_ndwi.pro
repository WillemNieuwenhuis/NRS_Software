pro nrs_water_index, image, water_index, outname = outname $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  ; Error checking
  indices = ['NDWI', 'LSWI', 'LSWI1', 'LSWI2']
  ibs = [[3, 1], [1, 4], [1, 4], [1, 5]]
  select = where(strupcase(water_index) eq indices, cnt)
  if cnt ne 1 then begin
    void = error_message('Unrecognized water index.')
    return
  endif
  sel_bands = ibs[*,select]
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then begin
    void = error_message('Could not open ' + image)
    return
  endif
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims $
                      , fname = fname $
                      , wl = wavelenghts, wavelenght_units = wu 
  inherit = envi_set_inheritance(fid, dims, /full)

  if nb ne 7 then begin
    void = error_message('Expected 7 reflectance bands, only ' + string(nb) + ' found.')
    return
  endif
  
  if (n_elements(wl) eq 0) || (wl eq -1) then begin
    b1 = sel_bands[0]
    b2 = sel_bands[1]
  endif else begin
; TODO: match wavelengths to determine required bands  
  endelse
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate water index: ' + water_index
  
  ; Start processing
  if n_elements(outname) eq 0 then begin
    outname = getOutname(fname, postfix = '_' + water_index, ext = '.dat')
  endif
  
  pos = indgen(nb)
  openw, unit, outname, /get_lun
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
    endif
    
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
    out_data = (float(data[*, b1] - data[*, b2])) / (data[*, b1] + data[*, b2])
    
    writeu, unit, out_data
  endfor
  
  out_type = size(out_data, /type)
  envi_setup_head, fname = outname $
        , data_type = out_type $
        , ns = ns, nl = nl, nb = 1 $
        , bnames = [water_index] $
        , interleave = 1 $  ; 1 == BIL
        , /write $
        , inherit = inherit
  
  close, unit
  free_lun, unit
  
end