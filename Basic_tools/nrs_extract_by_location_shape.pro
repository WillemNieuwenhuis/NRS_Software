function nrs_extract_generate_pattern, H, V, date, mod_product = mod_product
  compile_opt idl2, logical_predicate

  if n_elements(mod_product) eq 0 then mod_product = ''
  
  yy = fix(date / 10000)
  mm = fix((date mod 10000) / 100)
  dd = fix(date mod 100)
  doy = julday(mm, dd, yy) - julday(1, 1, yy)
  
  pattern = mod_product + '.' $
          + string(yy, format = '("A", i04)') + string(doy, format = '(i03)') + '.' $
          + string(H, format = '("h",i02)') + string(V, format = '("v",i02)')

  return, pattern
end

pro nrs_extract_add_bands_as_attributes, tile, myshape, num_attr, bandCount = nb
  compile_opt idl2, logical_predicate
  
  envi_open_file, tile, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = error_message('Fatal error, missing or corrupt data', /error, traceback = 0)
    stop
  endif
  envi_file_query, fid, nb = nb, data_type = dt
  envi_file_mng, id = fid, /remove
  
  attr_type = nrs_shape_attr_from(dt, width = width, dec = dec)
  ix = indgen(nb) + 1
  bnames = string(ix, format = '("Band_", i03)')
  for i = num_attr, num_attr + nb - 1 do begin
    name = bnames[i - num_attr]
    myshape->idlffshape::addattribute, name, attr_type, width, precision = dec
  endfor
end

function nrs_extract_at_pixel_location, tile, coordX, coordY, projection = proj, px = pixelX, py = pixelY, spectrum = spectrum
  compile_opt idl2

  valid = 1
  envi_open_file, tile, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    valid = 0
  endif

  ; get tile metadata: this will potentially differ for each tile, so repeat for each location
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims
  mi = envi_get_map_info(fid = fid, undef = undef_csy)

  ; translate to pixel location; also check if the pixel position
  ; lies inside the map -> if not skip to the next feature
  useProj = (n_elements(proj) gt 0) && (undef_csy eq 0 && mi.proj.type ne 0)
  if useProj then begin
    envi_convert_projection_coordinates, coordX, coordY, proj, crd_x, crd_y, mi.proj
  endif else begin
    crd_x = coordX
    crd_y = coordY
  endelse
  envi_convert_file_coordinates, fid, pixelX, pixelY, crd_x, crd_y  ; crd -> pixel
  if nrs_check_bounds(pixelX, pixelY, nl, ns) eq 0 then begin
    valid = 0
  endif else begin
    ; get the spectral values
    spectrum = envi_get_slice(fid = fid, line = pixelY, xs = pixelX, xe = pixelX, /bil)
    envi_file_mng, id = fid, /remove    ; close the tile again
  endelse
  
  return, valid
end


pro nrs_extract_by_location_shape, shapefile, folder, write_log = write_log, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2

  cancelled = 1
  
  ;Open the Shapefile in update mode
  nrs_openshapefile, shapefile, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info, /update

  nrs_set_progress_property, prog_obj, /start, title = 'Collecting locations'

  ; find all modis images
  files = nrs_find_images(folder, 'MCD43A4', ext = 'dat', /exclude_hdr)
  if n_elements(files) eq 0 then $
    files = nrs_find_images(folder, 'MCD43A4', ext = 'hdf', /exclude_hdr) ; original extension

  ; determine the fields of interest
  ih = where(attr_info.name eq 'H', h_cnt)
  iv = where(attr_info.name eq 'V', v_cnt)
  idt = where(attr_info.name eq 'ACQ_DATE', d_cnt)
  if (h_cnt lt 1) || (v_cnt lt 1) || (d_cnt lt 1) then begin
    void = error_message('Missing tile coordinate and / or acquisition date field(s)', /error, traceback = 0)
    return
  endif

  cancelled = 0
  
;  loc_info = {ID: 0L, X : 0.0, Y : 0.0, Hor : 0, Ver : 0, date : 0L, tile : ''}
  li_ar = []
  ; collect the correct tile for each location
  for i = 0, num_ent - 1 do begin
    if nrs_update_progress(prog_obj, i, num_ent) then begin
      cancelled = 1
      nrs_close_shapes, shapes = [myshape]
      return
    endif

    ; get the feature and its attributes
    feature = myshape->idlffshape::getentity(i, /attributes)
    ; get the point coordinate
    coordX = feature.bounds[0]
    coordY = feature.bounds[1]

    attr = feature.attributes
    hh = (*attr).(ih)         ; tile coordinate: H
    vv = (*attr).(iv)         ; tile coordinate: V
    datetime = (*attr).(idt)  ; acquisition date retrieved as yyyymmdd
    ; now determine which MODIS tile we need
    pattern = nrs_extract_generate_pattern(hh, vv, datetime)
    it = where(strpos(files, pattern) ge 0, t_cnt)
    if t_cnt eq 0 then continue ; no MODIS image available in this folder
     
    tile = files[it[0]]   ; only one tile expected, so take the first
    li_ar = [li_ar, {ID: i, X : coordX, Y : coordY, Hor : hh, Ver : vv, date : datetime, tile : tile}]
  endfor
  if n_elements(li_ar) eq 0 then begin
    void = error_message('No tiles matching the shapefile; nothing to do', /error, traceback = 0)
    return  ; nothing to do!
  endif
  
  ; setup index tables: indicate the features for which a tile is found
  hasTile = lonarr(num_ent)
  hasTile[li_ar.id] = 1
  valTiles = lonarr(num_ent)
  valTiles[li_ar.id] = lindgen(n_elements(li_ar))

  ; setup geographic system here, to prevent it being created multiple times  
  geo = envi_proj_create(/geographic)
  
  ; Create a new shape file (can only add attributes in empty table)
  ; and add attributes to the new shapefile to accommodate the values extracted from the MODIS tiles
  ; to do this we need to open one of the tiles to investigate the number of bands
  outputShapeName = getOutname(shapefile, postfix = '_extr')
  createnewshapefile, myshape, newshape, newname = outputShapeName
  nrs_extract_add_bands_as_attributes, li_ar[0].tile, newshape, num_attr, bandCount = bandCount
    
  ; iterate all locations matching a tile and do the extraction
  nrs_set_progress_property, prog_obj, /start, title = 'Extracting MODIS spectra' 
  for i = 0, num_ent - 1 do begin
    if nrs_update_progress(prog_obj, i, num_ent) then begin
      cancelled = 1
      nrs_close_shapes, shapes = [myshape, newshape]
      return
    endif
    
    ; Copy existing feature and attributes
    feature = myshape->idlffshape::getentity(i, /attributes)
    attr = feature.attributes
    newAttr = newshape->idlffshape::getattributes(/ATTRIBUTE_STRUCTURE)
    ; Copy the existing attributes
    for count = 0, num_attr - 1 do begin
      newAttr.(count) = (*attr).(count)
    end
    
    ; check if we have a tile for the current feature
    ; only read spectra from existing tiles
    if hasTile[i] eq 1 then begin
      ; get correct index in found tiles to retrieve the coord and tile name of current feature
      index = valTiles[i]
      featureID = li_ar[index].ID
      coordX = li_ar[index].X
      coordY = li_ar[index].Y
      tile = li_ar[index].tile
      
      isVal = nrs_extract_at_pixel_location(tile, coordX, coordY, spectrum = spectrum)
      if isVal eq 1 then begin    ; location OK, add spectrum in attributes
        ; set the spectrum values as attributes
        for count = num_attr, num_attr + bandCount - 1 do begin
          newAttr.(count) = spectrum[count - num_attr]
        end
      endif
    endif

    feature.attributes = ptr_new(newAttr)
    ptr_free, attr

    ; write the feature to the new shapefile
    newshape->idlffshape::putentity, feature

    ; cleanup for the feature structure
    myshape->idlffshape::destroyentity, feature

  end

  nrs_close_shapes, shapes = [myshape, newshape]

  if keyword_set(write_log) then begin  
    outname = getoutname(shapefile, post = '_log', ext = '.csv')
    openw, unit, outname, /get_lun
    printf, unit, 'f_id,tile'
    for i = 0, n_elements(li_ar) - 1 do begin
      printf, unit, string(li_ar[i].id, format = '(i0,",")') +  li_ar[i].tile
    endfor
    close, unit
    free_lun, unit
  endif

end

pro nrs_extract_by_location_run, shapefile, folder, write_log = write_log
  compile_opt idl2

  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Extract MODIS spectra by point location' $
    , /fast_loop $
    )
  prog_obj->Start

  nrs_extract_by_location_shape, shapefile, folder, write_log = write_log, prog_obj = prog_obj, cancelled = cancelled
  
  prog_obj->Destroy
end

pro nrs_extract_by_location_folder_batch, shapefile, folder_list, logging = logging
  compile_opt idl2

  folders = folder_list
  if nrs_get_file_extension(folder_list, /exclude_dot) eq 'lst' then begin
    folders = nrs_read_listfile(folder_list)
  endif

  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'blue' $
    , ysize = 15, title = 'Batch extract MODIS spectra' $
    , /fast_loop $
    )
  prog_obj->Start

  for f = 0, n_elements(folders) - 1 do begin
    prog_obj_inner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
      , ysize = 15, title = 'Extract MODIS spectra by point location' $
      , /fast_loop $
      )
    prog_obj_inner->Start
  
    nrs_extract_by_location_shape, shapefile, folders[f], write_log = logging, prog_obj = prog_obj, cancelled = cancelled
    
    prog_obj_inner->Destroy
  endfor

  prog_obj->Destroy
  
end

pro nrs_extract_by_location_shapefile_batch, shapefile_list, folder, logging = logging
  compile_opt idl2

  files = shapefile_list
  if nrs_get_file_extension(shapefile_list, /exclude_dot) eq 'lst' then begin
    files = nrs_read_listfile(shapefile_list)
  endif
  
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'blue' $
    , xsize = 200, ysize = 15 $
    , title = 'Batch extract MODIS spectra' $
    , /fast_loop $
    )
  prog_obj->Start

  for f = 0, n_elements(files) - 1 do begin
    void = nrs_update_progress(prog_obj, f, n_elements(files))
    
    prog_obj_inner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
      , xsize = 250, ysize = 15 $
      , title = 'Extract MODIS spectra by point location' $
      , level = 1 $
      , /fast_loop $
      )
    prog_obj_inner->Start

    nrs_extract_by_location_shape, files[f], folder, write_log = logging, prog_obj = prog_obj_inner, cancelled = cancelled

    prog_obj_inner->Destroy
  endfor

  prog_obj->Destroy

end

