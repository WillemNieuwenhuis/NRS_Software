pro nrs_filter_cooccurence, img, shapefile, kernel = kernel, outshape = outshape, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  if n_elements(kernel) eq 0 then kernel = 3
  
  envi_open_file, img, r_fid = fidIn, /no_interactive_query, /no_realize
  if fidIn eq -1 then begin
    void = error_message('Could not open: ' + img)
    return
  endif
  
  envi_file_query, fidIn, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt
  mi = envi_get_map_info(fid = fidIn, undefined = undef_csy)
  meta = envi_set_inheritance(fidIn, dims, /full)

  if n_elements(outname) eq 0 then outname = getoutname(img, postfix = '_cooc', ext = '.dat')

  envi_delete_rois, /all  ; make sure no rois are defined
  nrs_read_shape_polygons, shapefile, polygons = pols, att_names = att_names, hint_geo = hint_geo

  nr_pols = n_elements(pols.vertices)
  if nr_pols eq 0 then begin
    void = error_message('Shapefile does not contain polygons', /error, traceback = 0)
    return
  endif
  if n_elements(att_names) eq 0 then begin
    void = error_message('Shapefile has no attributes', /error, traceback = 0)
    return
  endif

  cancelled = 0

  ; Re-Open the Shapefile
  nrs_openshapefile, shapefile, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info

  ; Create a new shape file as output
  if n_elements(outshape) eq 0 then $
    outshape = getOutname(shapefile, postfix = '_cooc')
  createnewshapefile, myshape, newshape, newname = outshape

  ; create the extra attribute fields
  bnames_single = ['mean', 'var', 'homo', 'cont' $
    , 'diss', 'entr', 'moment', 'corr']
    
  bnames = []
  for b = 1, nb do begin
    bnames = [bnames, bnames_single]
  endfor
  bix = indgen(nb) + 1
  bixa = rebin(bix, nb, 8)
  bixa = string(transpose(reform(bixa, nb, 8)), format = '(i0)')
  bixa = reform(bixa, nb * 8, /over)
  bnames = string([transpose(bnames),transpose(bixa)],format='(a,"_",a)')

  attr_type = nrs_shape_attr_from(5, width = width, dec = dec)  ; 5 == double
  for i = num_attr, num_attr + n_elements(bnames) - 1 do begin
    name = bnames[i - num_attr]
    newshape->idlffshape::addattribute, name, attr_type, width, PRECISION = dec
  endfor

  nrs_set_progress_property, prog_obj, /start, title = 'Texture co-occurrence'

  pos = indgen(nb)  ; process all bands
  for p = 0, nr_pols - 1 do begin
    if nrs_update_progress(prog_obj, p, nr_pols) then begin
      cancelled = 1
      nrs_close_shapes, [myshape, newshape]
      return
    endif

    pol = *((pols.vertices)[p]) ; vertices of current polygon

    x = pol[0, *]
    y = pol[1, *]
    if ((undef_csy eq 0) && (mi.proj.type[0] ne 0)) && hint_geo then begin
      envi_convert_projection_coordinates, x, y, geo, crd_x, crd_y, mi.proj
      envi_convert_file_coordinates, fidIn, pixelX, pixelY, crd_x, crd_y
    endif else begin
      ; assume the same coordinate system
      envi_convert_file_coordinates, fidIn, pixelX, pixelY, x, y
    endelse
    pixelX = reform(round(pixelX), n_elements(pixelX))
    pixelY = reform(round(pixelY), n_elements(pixelY))

    new_roi = envi_create_roi(ns = ns, nl = nl, name = 'tree')
    envi_define_roi, new_roi, /polygon, xpts = pixelX, ypts = pixelY

    for b = 0, nb - 1 do begin
      rdata = envi_get_roi_data(new_roi, fid = fidIn, pos = b, addr = rix)
      if rix[0] eq -1 then continue 

      if b eq 0 then begin
        rix_ar = array_indices([ns,nl], rix, /dim)
        
        xymin = min(rix_ar, max = xymax, dim = 2)
        xmin = xymin[0]
        ymin = xymin[1]
        xmax = xymax[0]
        ymax = xymax[1]
        dims = [-1L, xmin, xmax, ymin, ymax]
        
        ix = rix_ar[0, *] - xmin
        iy = rix_ar[1, *] - ymin
        nsloc = xmax - xmin + 1
        nlloc = ymax - ymin + 1
        
        data = make_array(nsloc * nlloc, nb, type = dt)
        mask = intarr(nsloc, nlloc)
        mask[ix, iy] = 1
        mix = where(mask eq 1)
        mask = rebin(mask, nsloc, nlloc, nb)
      endif
      
      data[mix, b] = rdata
    endfor
    data = reform(data, nsloc, nlloc, nb, /over)
    data *= mask
    
    envi_write_envi_file, data, /no_open, /in_memory, r_fid = fidTree

    ; now start co-occurrence calculation
    envi_file_query, fidTree, dims = dimsTree

    envi_doit, 'texture_cooccur_doit', fid = fidTree, dims = dimsTree $
                 , direction = [1,1] $
                 , /in_memory $
                 , r_fid = fidOcc $
                 , method = intarr(8) + 1 $     ; calculate all texture measures
                 , kx = kernel, ky = kernel  $  ; square kernel
                 , g_levels = 64 $              ; quantization levels
                 , pos = pos $                  ; all bands
                 , /no_realize

    envi_file_query, fidOcc, ns = nsTree, nl = nlTree, nb = nbOcc, dims = dimsOcc
    ; result map has 8 * nb bands, grouped per input band
    ; per measure per input band the spatial information is aggregated to a single value (by averaging)
    
;    nrs_set_progress_property, prog_obj, /start, title = 'Adding co-occurrence as attributes'

    ; get the feature and its attributes
    feature = myshape->idlffshape::getentity(p, /attributes)
    ; get the point coordinate
    coordX = feature.bounds[0]
    coordY = feature.bounds[1]

    attr = feature.attributes
    newAttr = newshape->idlffshape::getattributes(/ATTRIBUTE_STRUCTURE)
    ; Copy the existing attributes
    for count = 0, num_attr - 1 do begin
      newAttr.(count) = (*attr).(count)
    end

    ; get the measure values
    for b = 0, nbOcc - 1 do begin
      dblock = envi_get_data(fid = fidOcc, dims = dimsOcc, pos = b)
      average = mean(dblock)
      ; set the average of the measure values as attributes
      count = num_attr + b
      newAttr.(count) = average
    endfor

    ; add the measures to the new output shape (polygon)
    feature.attributes = ptr_new(newAttr)
    ptr_free, attr

    ; write the feature to the new shapefile
    newshape->idlffshape::putentity, feature

    ; cleanup for the feature structure
    myshape->idlffshape::destroyentity, feature
    
    ; image cleanup: close the temp images
    envi_file_mng, id = fidTree, /remove
    envi_file_mng, id = fidOcc, /remove
  endfor

  nrs_close_shapes, shapes = [myshape, newshape]
  envi_file_mng, id = fidIn, /remove  ; close input image

end

