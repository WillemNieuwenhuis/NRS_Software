pro nrs_fields_to_stack, image, table, percentile $
        , outimage = outimage $
        , prog_obj = progressBar, cancelled = cancelled
  compile_opt idl2, logical_predicate

  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid lt 0 then return
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl
  mi_ref = envi_get_map_info(fid = fid, undefined = undef_csy)
  if undef_csy then void = temporary(mi_ref)

  if n_elements(outname) eq 0 then outname = getoutname(image, ext = '.dat', postfix = '_p')  

  nrs_load_class_image, fid, cldata = cldata, cnames = cnames, num_classes = nrclass $
                      , has_unclassified = has_unclassified
  if has_unclassified then nrclass--
  
  ; read the table
  ; 1st field = percentile (5, 25, 50)
  ; 2nd field = class (1..36)
  ; next fields are the values for each decade (10-day period) 
  fields = read_csv(table, header = header)
  nbOut = n_elements(header) - 2
  perc = fields.(0)
  ip = where(perc eq percentile, cntp)
  if cntp eq 0 then return
  
  nrs_set_progress_property, progressBar, /start, title = 'Calculate stack from attribute fields'
  openw, unit, outname, /get_lun
  outdata = fltarr(ns, nl, /nozero)
  cl = (fields.(1))[ip]
  for b = 0, nbOut - 1 do begin
    if nrs_update_progress(progressBar, b, nbOut) then begin
      return
    endif

    outdata[*] = 0
    fld = (fields.(b + 2))[ip]  ; get attribute data
    for c = 0, nrclass - 1 do begin
      ic = where(cldata eq (c + 1), cntc) ; select the class/id
      if cntc gt 0 then outdata[ic] = fld[c] ; replace the class/id with the attribute value
    endfor
    writeu, unit, outdata
    
  endfor
  
  meta = envi_set_inheritance(fid, dims, /spatial)

  envi_setup_head, fname = outname $
    , data_type = size(outdata, /type) $
    , /write $
    , interleave = 0 $  ; 0 == BSQ
    , nb = nbOut, nl = nl, ns = ns $
    , bnames = header[2:-1] $
    , data_ignore_value = 0.0 $
    , inherit = meta

  close, unit
  free_lun, unit

end
