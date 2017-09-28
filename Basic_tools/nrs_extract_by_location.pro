pro nrs_extract_by_location, shapefile, image, outshape = outshape, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fidHyper, /no_interactive_query, /no_realize
  if fidHyper eq -1 then return
  
  cancelled = 0
  
  envi_file_query, fidHyper, fname = filename, bnames = bandNames, nb = bandCount, $
    nl = lines, ns = columns, data_type = dt
  mi = envi_get_map_info(fid = fidHyper, undef = undef_csy)
  useGeo = (undef_csy eq 0 && mi.proj.type ne 0)
  if useGeo then $
    geo = envi_proj_create(/geographic)

  ;Open the Shapefile
  nrs_openshapefile, shapefile, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info

  ;Create a new shape file
  createnewshapefile, myshape, newshape, newname = outshape

  attr_type = nrs_shape_attr_from(dt, width = width, dec = dec)
  ix = indgen(bandCount) + 1
  bnames = string(ix, format = '("Band_", i03)')
  for i = num_attr, num_attr + bandCount - 1 do begin
    name = bnames[i - num_attr]
    newshape->idlffshape::addattribute, name, attr_type, width, PRECISION = dec
  endfor

  ; Start copying the entities and attributes
  ; Read the shape file entity, and if it is a point feature
  ; get the band values from the image file at that location
  ; then store the feature in the new shape file, copy
  ; the feature attributes and also add the band values as
  ; new attributes

  nrs_set_progress_property, prog_obj, /start, title = 'Copying image spectra as attributes'
  
  for i = 0, num_ent - 1 do begin
    if nrs_update_progress(prog_obj, i, num_ent) then begin
      cancelled = 1
      nrs_close_shapes, [myshape, newshape]
      return
    endif
    
    ; get the feature and its attributes
    feature = myshape->idlffshape::getentity(i, /attributes)
    ; get the point coordinate 
    coordX = feature.bounds[0]
    coordY = feature.bounds[1]
    ; translate to pixel location; also check if the pixel position
    ; lies inside the map -> if not skip to the next feature
    if useGeo then begin
      envi_convert_projection_coordinates, coordX, coordY, geo, crd_x, crd_y, mi.proj
    endif else begin
      crd_x = coordX
      crd_y = coordY
    endelse
    envi_convert_file_coordinates, fidHyper, pixelX, pixelY, crd_x, crd_y
    if nrs_check_bounds(pixelX, pixelY, lines, columns) eq 0 then continue

    attr = feature.attributes
    newAttr = newshape->idlffshape::getattributes(/ATTRIBUTE_STRUCTURE)
    ; Copy the existing attributes
    for count = 0, num_attr - 1 do begin
      newAttr.(count) = (*attr).(count)
    end

    ; get the spectral values
    spectrum = envi_get_slice(fid = fidHyper, line = pixelY, xs = pixelX, xe = pixelX, /bil)
    ; set the spectrum values as attributes
    for count = num_attr, num_attr + bandCount - 1 do begin
      newAttr.(count) = spectrum[count - num_attr]
    end

    feature.attributes = ptr_new(newAttr)
    ptr_free, attr

    ; write the feature to the new shapefile
    newshape->idlffshape::putentity, feature

    ; cleanup for the feature structure
    myshape->idlffshape::destroyentity, feature
  end

  nrs_close_shapes, shapes = [myshape, newshape]

end
