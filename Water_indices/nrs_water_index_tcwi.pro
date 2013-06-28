pro nrs_water_index_TCWI, image, outname = outname $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
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
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate water index: TCWI'
  
  ; Start processing
  if n_elements(outname) eq 0 then begin
    outname = getOutname(fname, postfix = '_tcwi', ext = '.dat')
  endif
  
  c1 = 0.10839
  c2 = 0.0912
  c3 = 0.5065
  c4 = 0.404
  c5 = 0.241
  c6 = 0.4658
  c7 = 0.5306
  pos = indgen(nb)
  openw, unit, outname, /get_lun
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
      return
    endif
    
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    out_data = c1 * data[*, 0] + c2 * data[*, 1] + c3 * data[*, 2] + c4 * data[*, 3] $
                               - c5 * data[*, 4] - c6 * data[*, 5] - c7 * data[*, 6]
    
    writeu, unit, out_data
  endfor
  
  out_type = size(out_data, /type)
  envi_setup_head, fname = outname $
        , data_type = out_type $
        , ns = ns, nl = nl, nb = 1 $
        , bnames = ['TWCI'] $
        , interleave = 1 $  ; 1 == BIL
        , /write $
        , inherit = inherit
  
  close, unit
  free_lun, unit
  
end