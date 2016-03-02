pro nrs_zonal_fraction_batch, in_folder, shapefile, attribute = attribute $
                          , out_table = out_table $
                          , out_folder = out_folder, extension = extension $
                          , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  if n_elements(out_folder) eq 0 then out_folder = in_folder
  fi_in = file_info(in_folder)
  fi_out = file_info(out_folder)
  if ~fi_out.directory then out_folder = in_folder

  if fi_in.directory then begin
    files = nrs_find_images(in_folder, extension = extension)
    nr_files = n_elements(files)
    if nr_files eq 0 then begin
      void = error_message('No files found or match the file extension', traceback = 0)
      return
    endif
    if strlowcase(nrs_get_file_extension(shapefile)) ne '.shp' then begin
      void = error_message('No shapefile specified', traceback = 0)
      return
    endif

    for f = 0, n_elements(files) - 1 do begin
      if nrs_update_progress(prog_obj, f, nr_files) then return
      
      nrs_zonal_fraction_shape, files[f], shapefile, attribute = attribute, stat_table = out_table
    endfor
    
  endif
end

pro nrs_zonal_fraction_read_shape, shapes, polygons = pols, att_names = att_names
  compile_opt idl2, logical_predicate

  pols = []
  att_names = []

  if strlowcase(nrs_get_file_extension(shapes)) ne '.shp' then begin
    void = error_message('No shapefile specified')
    return
  endif

  ; open the input shapefile
  shape = obj_new('IDLffShape', shapes)

  ; Get the number of entities and the entity type.
  shape->idlffshape::getproperty, n_entities = num_ent, $
    entity_type = ent_type, n_attributes = num_attr

  if num_ent le 0 then begin
    obj_destroy, shape
    void = error_message('No features found in the shapefile')
    return
  endif
  
  pnt_types = [5, 15, 25]
  ix = where(ent_type eq pnt_types, pt_cnt)
  if pt_cnt eq 0 then begin
    obj_destroy, shape
    void = error_message('Only polygon features supported')
    return
  endif

  if num_attr gt 0 then begin
    shape->IDLffShape::GetProperty, attribute_info = attr_info
    att_names = attr_info.name
  endif
  pols = shape->IDLffShape::GetEntity(/all, /attributes)

  obj_destroy, shape
end

pro nrs_zonal_fraction_shape, image, shapefile, attribute = attribute $
          , outname = outname, stat_table = stat_table
  compile_opt idl2, logical_predicate

  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then begin
    void = error_message('Could not open: ' + image)
    return
  endif
  envi_file_query, fid, ns = ns, nl = nl, dims = dims
  mi = envi_get_map_info(fid = fid, undefined = undef_csy)
  meta = envi_set_inheritance(fid, dims, /full)

  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_znal', ext = '.dat')

  envi_delete_rois, /all  ; make sure no rois are defined
  nrs_zonal_fraction_read_shape, shapefile, polygons = pols, att_names = att_names

  minx = min((pols.bounds)[0, *])  
  miny = min((pols.bounds)[1, *])
  maxx = max((pols.bounds)[4, *])
  maxy = max((pols.bounds)[5, *])
  hint_geo = ((abs(maxx) le 360.0 && abs(minx) le 360.0) $
    && (abs(maxy) le 360.0 && abs(miny) le 360.0))

  nr_pols = n_elements(pols.vertices)
  outimg = fltarr(ns, nl)
  aggr = fltarr(nr_pols)
  att_names = strlowcase(att_names)
  attr_ix = where(strlowcase(attribute) eq att_names, ix_cnt)
  if ix_cnt eq 0 then attr_ix = 0 ; fallback: use first attribute if none specified
  geo = envi_proj_create(/geographic)
  valid = bytarr(nr_pols)
  for p = 0, nr_pols - 1 do begin
    pol = *((pols.vertices)[p]) ; vertices of current polygon
    aval = (*(pols.attributes)[p]).(attr_ix) ; value of selected attribute of current polygon
    new_roi = envi_create_roi(ns = ns, nl = nl, name = string(aval))

    x = pol[0, *]
    y = pol[1, *]
    if (undef_csy eq 0 && mi.proj.type[0] ne 0) && hint_geo then begin
      envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
      envi_convert_file_coordinates, fid, pixelX, pixelY, crd_x, crd_y
    endif else begin
      ; assume the same coordinate system
      envi_convert_file_coordinates, fid, pixelX, pixelY, x, y
    endelse
    pixelX = reform(round(pixelX), n_elements(pixelX))
    pixelY = reform(round(pixelY), n_elements(pixelY))

    envi_define_roi, new_roi, /polygon, xpts = pixelX, ypts = pixelY 
    rdata = envi_get_roi_data(new_roi, fid = fid, pos = 0, addr = rix)
    if rix[0] eq -1 then continue 

    valid[p] = 1
    h = histogram(rdata, min = 0)
    if n_elements(h) lt 2 then continue
    aggr[p] = h[1] / total(h) ; calculate percentage non-zero values
    outimg[rix] = aggr[p]
  endfor
  
  envi_write_envi_file, outimg, out_name = outname, inherit = meta, /no_open, /no_copy

  if n_elements(stat_table) gt 0 then begin
    fi = file_info(stat_table, /noexpand_path)
    openw, unit, stat_table, /get_lun, /append
    if ~fi.exists or (fi.size eq 0) then begin
      ; write header if file is still empty
      printf, unit, attribute + ',mean,filename'
    endif
    for p = 0, nr_pols - 1 do begin
      if valid[p] eq 1 then $
        printf, unit, string((*(pols.attributes)[p]).(attr_ix)) $
          + string(aggr[p], format = '(",", f0.3, ",")') $
          + file_basename(image)
    endfor

    close, unit
    free_lun, unit
  endif

end

pro nrs_zonal_fraction_roi, image, roi_file, outname = outname, stat_table = stat_table
  compile_opt idl2, logical_predicate
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then begin
    void = error_message('Could not open: ' + image)
    return
  endif
  envi_file_query, fid, ns = ns, nl = nl, dims = dims
  meta = envi_set_inheritance(fid, dims, /full)
  
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_znal', ext = '.dat')
  
  envi_delete_rois, /all  ; make sure no rois are defined
  envi_restore_rois, roi_file
  roiids = envi_get_roi_ids(fid = fid, roi_names = attr_ids)
  nr_regions = n_elements(roiids)
  img_name = file_basename(image) 
  
  aggr = fltarr(nr_regions)
  rid = lonarr(nr_regions)
  outimg = fltarr(ns, nl)
  for r = 0, nr_regions - 1 do begin
    parts = strsplit(attr_ids[r], '(=)', /extract)
    rid[r] = long(parts[2]) ; the polygon / roi attribute ID
    rdata = envi_get_roi_data(roiids[r], fid = fid, pos = 0, addr = ix)
    h = histogram(rdata, min = 0)
    if n_elements(h) lt 2 then continue
    aggr[r] = h[1] / total(h) ; calculate percentage non-zero values
    outimg[ix] = aggr[r]
  endfor
  
  envi_write_envi_file, outimg, out_name = outname, inherit = meta, /no_open, /no_copy

  if n_elements(stat_table) gt 0 then begin
    fi = file_info(stat_table, /noexpand_path)
    openw, unit, stat_table, /get_lun, /append
    if ~fi.exists or (fi.size eq 0) then begin
      ; write header if file is still empty
      printf, unit, 'FID,mean,filename'
    endif
    for r = 0, nr_regions - 1 do begin
      printf, unit, string(rid[r], format = '(i0, ",")') $
                  + string(aggr[r], format = '(f0.3, ",")') $
                  + img_name
    endfor
    
    close, unit
    free_lun, unit
  endif
    
  
end
