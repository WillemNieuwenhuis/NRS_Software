pro nrs_water_index, image, water_index, outname = outname $
                  , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  ; Error checking
  indices = ['NDWI', 'LSWI', 'LSWI1', 'LSWI2', 'TCWI']
  select = where(strupcase(water_index) eq indices, cnt)
  if cnt ne 1 then begin
    void = error_message('Unrecognized water index.')
    return
  endif

  if select eq 4 then begin
    nrs_water_index_TCWI, image, outname = outname $
                  , prog_obj = prog_obj, cancelled = cancelled
  endif else begin
    nrs_water_index_ratio, image, water_index, outname = outname $
                  , prog_obj = prog_obj, cancelled = cancelled
  endelse
  
end
