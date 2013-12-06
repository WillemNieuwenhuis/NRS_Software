;+
; :description:
;    Recalculate the data values with an offset and scale factor. Optionally
;    change the data type.
;    
;    The formula:
;    
;     Offset before scale:  <i>out = (offset + input) * scale</i>
;     
;     Scale before offset:  <i>out = scale * input + offset</i>
;
; :params:
;    img_name : in
;      The filename of the image
;
; :keywords:
;    outname : in, optional
;      The name of the output file
;    scale : in, optional, default = 1.0
;      The multiplication factor
;    offset : in, optional, default = 0.0
;      The offset
;    off_before_scale : in, optional, default = no
;      If set first add the offset to the value before multiplication.
;      
;      Otherwise first multiply with the value and then add the offset
;    data_type : in, optional, default = same as input
;      Specify the data type of the output (byte, int, long, float or double)
;    prog_obj : in, optional
;      Progress indicator object
;    cancelled : out, optional
;      Indicate failure or user break
;
; :author: nieuwenhuis
; 
; :history:
;   Changes::
;     21 Feb 2012: nieuwenhuis, created
;     24 Oct 2013: nieuwenhuis, added data_type keyword
;                               Internally use highest precision to perform the calculation
;-
pro nrs_scale_offset, img_name, outname = outname $
                    , scale = scale, offset = offset, off_before_scale = offset_before_scale $
                    , data_type = dt_out $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1

  if n_elements(scale) eq 0 then scale = 1.0
  if n_elements(offset) eq 0 then offset = 0.0
  obs = keyword_set(offset_before_scale)
  
  envi_open_file, img_name, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq - 1 then begin
    void = dialog_message('Image open failed / not found', /error)
    return
  endif

  envi_file_query, fid, data_ignore_value = nodata, ns = ns, nl = nl, nb = nb $
                 , dims = dims $
                 , interleave = interleave $
                 , xstart = xs, ystart = ys $
                 , bnames = bnames, data_type = dt_in
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef then delvar, mi
;  envi_file_mng, fid, /remove
  
  dtx = where(dt_in eq [1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 15], dt_cnt)
  if n_elements(dt_out) eq 0 then dt_out = 0; 4  ; assume float
  dtoutx = where(dt_out eq [1, 2, 3, 4, 5, 6, 9, 12, 13, 14, 15], dtoutx_cnt)
  
  if dt_cnt eq 0 then begin
    void = dialog_message('Image is not numeric', /error)
    return
  endif
  if dtoutx_cnt eq 0 then dt_out = dt_in

  cancelled = 0

  if n_elements(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(img_name, postfix = '_offsca', ext = '.dat')
  endif

  nrs_set_progress_property, prog_obj, /start, title = 'Applying scale / offset'
  
  ; open the input and output files
  openr, unit_in, img_name, /get_lun
  openw, unit, outname, /get_lun

  limits = nrs_minmax_from_datatype(dt_out)
  nodata_out = limits[0]
  data_in = make_array(ns, nl, type = dt_in)
  data_out = make_array(ns, nl, type = dt_out)
  
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then begin
      close, unit_in
      close, unit
      free_lun, unit_in
      free_lun, unit
      return
    endif
    
    readu, unit_in, data_in
    ix = where(data_in eq nodata, count)
    ; make sure to perform calculations in the higher precision data type
    if dt_in lt dt_out then data = fix(data_in, type = dt_out) $
    else data = data_in
    
    if obs then begin
      ; offset before scale
      data += offset
      data *= scale
    endif else begin
      data *= scale
      data += offset
    endelse
    
    if dt_out lt dt_in then $
      data = (data < limits[1]) > limits[0]
    
    data_out[*] = fix(data, type = dt_out)
    if count gt 0 then data_out[ix] = nodata_out
    
    writeu, unit, data_out
  endfor
  close, unit_in
  close, unit
  free_lun, unit_in
  free_lun, unit
  
  envi_setup_head, fname = outname $
        , data_type = dt_out $
        , /write $
        , xstart = xs, ystart = ys $
        , interleave = interleave $
        , nb = nb, nl = nl, ns = ns $
        , bnames = bnames $
        , map_info = mi $
        , data_ignore_value = nodata_out
end

