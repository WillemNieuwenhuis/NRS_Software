pro nrs_sentinel_config, sen2cor = sen2cor
  compile_opt idl2, logical_predicate

  config_path = getenv('homedrive') + getenv('homepath') + path_sep() + 'nrs_sentinel'
  isOK = file_test(config_path, /dir)
  if ~isOK then begin
    file_mkdir, config_path
  endif

  

end


pro nrs_sentinel2_run_sen2cor, folder
  compile_opt idl2, logical_predicate

  
  config_path = getenv('homedrive') + getenv('homepath') + path_sep() + 'nrs_sentinel'
  isOK = file_test(config_path, /dir)
  if ~isOK then begin
    file_mkdir, config_path 
  endif
  
  
  pushd, folder

  sen2cor = "E:\temp\Sen2Cor-2.4.0-win64\L2A_Process.bat " + folder

  spawn, sen2cor, unit = unit
  print, 'File handle=', unit
  ans = bytarr(1)
  line = bytarr(250)
  linpos = 0
  while not eof(unit) do begin
    readu, unit, ans
    line[linpos] = ans
    if (ans ne 13) && (ans ne 10) then linpos++
    if ans eq 10 then begin
      print, string(line)
      line[*] = 0
      linpos = 0
    endif
  endwhile

  free_lun, unit

  popd
end

function nrs_sentinel2_level, base_folder, level_str = level_str
  compile_opt idl2, logical_predicate

  level_str = ''
  level = -1
  
  pattern = 'MTD_MSIL'
  meta_file = nrs_find_images(base_folder, pattern, ext = 'xml')
  if n_elements(meta_file) eq 1 then begin
    meta_file = meta_file[0]
    pos = strpos(meta_file, pattern)
    if pos gt 0 then begin
      level_str = strmid(meta_file, pos + strlen(pattern), 2)
      level = fix(level_str)
    endif
  endif
  
  return, level
end

;+
; :Description:
;    Extract metadata from the sentinel granule
;
; :Params:
;    xml : input, required
;      The granule metadata file name
;
; :Keywords:
;    map_info : out
;      An array of mapinfo records for the different scaled images. Has the same number of elements as res_dims
;    res_dims : out
;      An array with the dimensions of the different scaled images. Has the same number of elements as res_dims
;    sense_date : out
;      The date of the image capture.
;
; :Author: nieuwenhuis
;-
pro nrs_sentinel_xml_csy, xml, map_info = map_info, res_dims = res_dims, sense_date = sense_date
  compile_opt idl2, logical_predicate

  p = obj_new('IDLffXMLDOMDocument')
  p->load, filename = xml, schema_checking = 0, /quiet

  oTopLevel = p->getDocumentElement() ;return root IDLffXMLDOMDocument

  ; get the sensing date (at start time)
  node = otoplevel->getElementsByTagName('SENSING_TIME')
  item = node->item(0)
  dt = (item->getFirstChild())->getNodeValue()
  parts = strsplit(dt, 'T', /extract)
  parts = strsplit(parts[0], '-', /extract)
  sense_date = strjoin(parts)

  ; get the projection information (EPSG number)
  prjnode = oTopLevel->getElementsByTagName('HORIZONTAL_CS_CODE')
  prjitem = prjnode->item(0)
  prj = (prjitem->getFirstChild())->getNodeValue()
  parts = strsplit(prj, ':', /extract)
  epsg = long(parts[1])

  ; get projection details
  metaList = oTopLevel->getElementsByTagName('Tile_Geocoding')
  ; how many KeyMeta tags in documents
  metaElement = metaList->item(0)

  ; get the tiepoints for the different resolutions
  positions = metaElement->getElementsByTagname('Geoposition')
  map_info = []
  for c = 0L, positions->getLength() - 1L do begin
    node = positions->item(c)
    items = node->getElementsByTagname('*')
    attr = node->getAttributes()
    attritem= attr->item(0)
    ps = fix(attritem->getNodeValue())
    ps = [ps, ps]
    mc = dblarr(4)
    mc[0:1] = 0
    for k = 0L, items->getLength() - 1L do begin
      item = items->item(k)
      name = item->getTagname()
      dataList = item->getFirstChild()
      value = dataList->getNodeValue()
      if name eq 'ULX' then mc[2] = double(value)
      if name eq 'ULY' then mc[3] = double(value)
    endfor
    map_info = [map_info, envi_map_info_create(type = 42, pe_coord_sys_code = epsg, mc = mc, ps = ps)]
  endfor

  ; get the X and Y dimensions for the different resolutions
  positions = metaElement->getElementsByTagname('Size')
  res_dims = lonarr(2,3)
  for c = 0L, positions->getLength() - 1L do begin
    node = positions->item(c)
    items = node->getElementsByTagname('*')
    attr = node->getAttributes()
    attritem = attr->item(0)
    ps = fix(attritem->getNodeValue())
    ix = where(ps eq [10, 20, 60])
    for k = 0L, items->getLength() - 1L do begin
      item = items->item(k)
      name = item->getTagname()
      dataList = item->getFirstChild()
      value = dataList->getNodeValue()
      if name eq 'NCOLS' then res_dims[k, ix] = long(value)
      if name eq 'NROWS' then res_dims[k, ix] = long(value)
    endfor
  endfor

  obj_destroy, p
end

pro nrs_sentinel_meta, xml, scale = scale
  compile_opt idl2, logical_predicate

  p = obj_new('IDLffXMLDOMDocument')
  p->load, filename = xml, schema_checking = 0, /quiet

  oTopLevel = p->getDocumentElement() ;return root IDLffXMLDOMDocument

  ; get the sensing date (at start time)
  node = otoplevel->getElementsByTagName('L2A_BOA_QUANTIFICATION_VALUE')
  if node->getlength() eq 0 then return
  
  item = node->item(0)
  scale = long( (item->getFirstChild())->getNodeValue())
  
  obj_destroy, p
end


;+
; :Description:
;    Determine the geotiff keys from the map_info from the image handle.
;    This function only performs well with metadata extracted from Sentinel2 L2A images as
;    it assumes UTM
;
; :Params:
;    fid : in, required
;      Open image handle to retrieve the map_info from
;
;
; :Author: nieuwenhuis
; :History:
;   - October 2017, WN, created
;-
function nrs_convert_S2_geokeys, fid
  compile_opt idl2, logical_predicate

  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb
  mi = envi_get_map_info(fid = fid, undef = undef)
  envi_convert_file_coordinates, fid, 0, 0, xm, ym, /to_map
  syscode = mi.proj[0].pe_coord_sys_code
  if syscode eq 0 then begin
    zone = mi.proj[0].params[0]
    hemi = mi.proj[0].params[1]
    syscode = 32600 + zone + (hemi ne 0 ? 100 : 0)
  endif

  geokeys = { $
    ModelPixelScaleTag: [mi.ps[0], mi.ps[1], 0d], $
    ModelTiepointTag: [0, 0, 0, xm, ym, 0], $
    GTModelTypeGeoKey: 1, $           ; (ModelTypeProjected)
    GTRasterTypeGeoKey: 1, $          ; (RasterPixelIsArea)
    GeogLinearUnitsGeoKey: 9001, $    ; (Linear_Meter)
    GeogAngularUnitsGeokey: 9102, $   ; (Angular Degree)
    ProjectedCSTypeGeoKey: syscode $  ; UTM code
  }

  return, geokeys
end

pro nrs_sentinel_product_error, folder = folder, multiple = multiple
  compile_opt idl2, logical_predicate

  if multiple then $
    print, 'No Sentinel L2 product found' $
  else $
    print, 'No Sentinel L2 product found in: ' + folder

end

 

;+
; :Description:
;    Add ENVI headers to all the JP2 images in the Sentinel2 L2A product. The ENVI headers will contain
;    the spatial reference. It can handle both S2A and S2B
;
; :Params:
;    folders : in, required
;      A string or array of strings indicating the full path(s) of sentinel folder(s). 
;      The base folder of the sentinel datastructure. The GRANULE folder should be found in this base folder
;
;
; :Author: nieuwenhuis
; 
; :History:
;   - august 2017: created
;-
pro nrs_convert_S2L2_to_S2ENVI, folders
  compile_opt idl2, logical_predicate
  
  multiple = n_elements(folders) gt 1
  for fold = 0, n_elements(folders) - 1 do begin
    
    folder = folders[fold]
    if multiple then print, 'starting: ', folder
    
    ; find tile data folder and extract meta data
    tile_folder = file_search(count = fc, folder + '\granule\s2*')    ; old structure
    if fc eq 0 then tile_folder = file_search(folder + '\granule\l2*')  ; new structure
    meta_file = nrs_find_images(tile_folder, ext = 'xml')
    if n_elements(meta_file) ne 1 then begin
      nrs_sentinel_product_error, folder = folder, multiple = multiple
      continue
    endif
    meta_xml = meta_file[0]
    nrs_sentinel_xml_csy, meta_xml, map_info = map_info, res_dims = res_dims, sense_date = sense_date
  
    folder_10m = tile_folder + path_sep() + 'IMG_DATA\R10m'
    folder_20m = tile_folder + path_sep() + 'IMG_DATA\R20m'
    folder_scl  = tile_folder + path_sep() + 'IMG_DATA'    ; old structure
    if fc eq 0 then folder_scl  = tile_folder + path_sep() + 'IMG_DATA\R20m'  ; new structure
    flist = [folder_10m[0], folder_20m[0], folder_scl[0]]
    pattern = ['', '', 'SCL']
  
    extract_bands = []
    scl_file = ''
    ; Add ENVI headers if needed
    for fo = 0, n_elements(flist) - 1 do begin
      files = nrs_find_images(flist[fo], pattern[fo], ext = 'jp2')  ; select all spectral bands
      if n_elements(files) eq 0 then begin
        if multiple then $
          print, 'No Sentinel L2A product found' $
        else $
          print, 'No Sentinel L2A product found in: ' + tile_folder
  
        break
      endif
      for f = 0, n_elements(files) - 1 do begin
        mi = map_info[fo]
        select = nrs_extract_select(files[f], scl = scl)
        if select then extract_bands = [extract_bands, files[f]]
        if scl then begin
          scl_file = files[f]
          mi = map_info[1]  ; force to 20m
        endif
        nrs_sentinel_jp2_add_header, files[f], mapinfo = mi
      endfor
    endfor
    
  endfor

end

;+
; :Description:
;    Build a stack from Sentinel2 images. This is done for a single resolution.
;    Optionally a conversion from DN to BOA reflectance values is done
;
; :Params:
;    folder : in, required
;      The base folder of the Sentinel product
;    gain : in, optional
;      The correction factor to get from DN to reflectance
;
; :Keywords:
;    dn_to_reflection : in, optional, default = no
;      If set and yes, perform the DN to reflectance correction
;
; :Author: nieuwenhuis
;-
pro nrs_sentinel_stack, folder, gain, resolution = resolution, dn_to_reflection = dn_to_reflection
  compile_opt idl2, logical_predicate

  ; find tile data folder and extract meta data
  tile_folder = file_search(count = fc, folder + '\granule\s2*')    ; old structure
  if fc eq 0 then tile_folder = file_search(folder + '\granule\l2*')  ; new structure

  resol_lut = [10, 20, 60]
  imgfolder = [tile_folder, tile_folder, tile_folder] + ['\IMG_DATA\R10m', '\IMG_DATA\R20m', '\IMG_DATA\R60m']

  res_ix = where(resol_lut eq resolution)
  folder_res = imgfolder[res_ix]
  pattern = '_B'  ; only select the regular spectral bands
  files = nrs_find_images(folder_res, pattern, ext = 'jp2')

  nrfiles = n_elements(files)
  bpos = strpos(files, '_B')
  bnam = strmid(reform(files, 1, nrfiles), reform(bpos + 2, 1, nrfiles), 2)
  six = sort(fix(bnam))
  files = files[six] ; put the bands in correct order

  ; prepare arrays for layer stacking
  case res_ix of
    0 : bnames = ['02', '03', '04', '08']
    1 : bnames = ['02', '03', '04', '05', '06', '07', '8A', '11', '12']
    2 : bnames = ['01', '02', '03', '04', '05', '06', '07', '8A', '9', '11', '12']
  endcase

  ; stack bands
  name = file_basename(files[0], '.jp2')
  ix = strpos(name, 'B0')
  name = strmid(name, 0, ix) + 'stack' + strmid(name, ix + 3)
  outname = out_folder + path_sep() + name + '.dat'
  fids = []
  for f = 0, n_elements(files) - 1 do begin
    envi_open_file, files[f], r_fid = fid_cur, /no_interactive_query, /no_realize
    if f eq 0 then begin
      envi_file_query, fid_cur, dims = dims, data_type = dt
      mi = envi_get_map_info(fid = fid_cur)
    endif

    fids = [fids, fid_cur]
  endfor

  nr_bands = n_elements(fids)
  all_dims = rebin(dims, 5, nr_bands)
  pos = intarr(nr_bands)
  tmpfile = nrs_get_temporary_filename(root = folder)
  if keyword_set(dn_to_reflection) then sname = tmpfile else sname = outname
  envi_doit, 'envi_layer_stacking_doit', fid = fids, pos = pos $
    , out_name = sname $
    , dims = all_dims $
    , out_dt = dt, out_bname = bnames $
    , out_proj = mi.proj, out_ps = mi.ps $
    , r_fid = fid_stack $
    , /invisible, /no_realize

  for fi = 0, nr_bands - 1 do $
    envi_file_mng, id = fids[fi], /remove

  if keyword_set(dn_to_reflection) then begin
    ; fid_stack now points to the temp output. This is used as input for the gain correction giving a new output
    envi_doit, 'gainoff_doit', dims = dims, fid = fid_stack $
      , gain = fltarr(nr_bands) + gain, offset = fltarr(nr_bands), pos = lindgen(nr_bands) $
      , out_dt = dt, out_name = outname

    envi_file_mng, id = fid_stack, /delete   ; close and delete the temp file
  endif else begin
    envi_file_mng, id = fid_stack, /remove   ; just close the new file
  endelse

end


;+
; :Description:
;    Create ENVI stacks for the 10m and 20m atmospherically corrected Sentinel2 bands (L2A product).
;    The 10m stack contains: B2, B3, B4, B8, in that order
;    The 20m stack contains: B2, B3, B4, B5, B6, B7, B8A, B11, B12, in that order.
;    
;    The data in the corrected bands is still in DN. Optionally this can be corrected by reading the 
;    scale factor from the metadata and applying the correct gain to get actual reflectance values. 
;    
;    The band names reflect the Sentinel2 band.
;    
;    It is assumed that the Sentinel bands already have ENVI headers. These can be added by using <i>nrs_S2_L2_to_ENVI</i>
;
; :Params:
;    folders : in, required
;      The base folder of the sentinel datastructure. The GRANULE folder should be found in this base folder
;      This is either a single fully foldername or an array with full specified foldernames
;    out_folder : out, required
;      folder to collect all results; it should already exist
;
; :Keywords:
;    dn_to_reflection : in, optional, default = no
;      Convert the DN numbers to actual reflection values   
;
; :Author: nieuwenhuis

; :History:
;   - august 2017: created
;-
pro nrs_convert_S2ENVI_to_ENVIstack, folders, out_folder, dn_to_reflection = dn_to_reflection
  compile_opt idl2, logical_predicate

  for fold = 0, n_elements(folders) - 1 do begin
    
    folder = folders[fold]

    gain = 1
    if keyword_set(dn_to_reflection) then begin
      meta_top = nrs_find_images(folder, 'SAFL2', ext = 'xml')
      if n_elements(meta_top) ne 1 then begin
        nrs_sentinel_product_error, folder = folder, multiple = multiple
        continue
      endif
      nrs_sentinel_meta, meta_top[0], scale = scale
      gain = 1.0 / scale
    endif

    ; only stack 10m and 20m, skip 60m
    nrs_sentinel_stack, folder, gain, resolution = 10, dn_to_reflection = dn_to_reflection
    nrs_sentinel_stack, folder, gain, resolution = 20, dn_to_reflection = dn_to_reflection

  endfor

end

;+
; :description:
;    Convert ENVI images to geotif files. The files are specified by the folder they reside in.
;    Assumption: they are created of Sentinel 2 images and therefore are in UTM projection
;    The projection information is determined from the metadata and set into the tiff projection info.
;
; :params:
;    folder : in, required
;      The folder containing the ENVI images to convert
;    out_folder : out, required
;      The folder to store the converted tiff files 
;      
; :Keywords:
;    verbose : in, optional, default = yes
;      if set (to non-zero) then show warnings and errors. If set to zero then do not show warnings and errors 
;
; :history:
;   september 2016: WN, created
;   august 2017: WN, determine resolution from donor tiff
;   october 2017: WN, remove the need for a donor tiff file to write the geokeys
;
; :author: nieuwenhuis
;-
pro nrs_convert_S2ENVI_to_tiff, folder, out_folder, verbose = verbose
  compile_opt idl2, logical_predicate

  if n_elements(out_folder) eq 0 then begin
    if ~keyword_set(verbose) then $
      void = error_message('Please specify an existing output folder', title = 'Convert ENVI to TIFF', /error)
    return
  endif
  
  pattern = '_10m,_20m,_60m'
  files = nrs_find_images(folder, pattern, ext = 'dat')

  for f = 0, n_elements(files) - 1 do begin
    outname = getoutname(files[f], postfix = '_pcd', ext = '.tif')
    name = file_basename(outname)
    outname = out_folder + path_sep() + name
    envi_open_file, files[f], r_fid = fid, /no_interactive_query, /no_realize
    envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb

    geokeys = nrs_convert_S2_geokeys(fid)

    grid = make_array(nb, ns, nl, /uint)
    for b = 0, nb - 1 do begin
      data = envi_get_data(fid = fid, dims = dims, pos = b)
      grid[b, *, *] = data
    endfor
    write_tiff, outname, grid, /short, geotiff = geokeys

    envi_file_mng, id = fid, /remove

  endfor
end

pro nrs_convert_S2TIFF_subset_roi, files, out_folder, clip_roi, verbose = verbose
  compile_opt idl2, logical_predicate

  if n_elements(files) eq 0 then return

  ok = query_tiff(files[0], tags, image_index = 0, geo = geokeys)
  envi_open_file, files[0], r_fid = fid, /no_interactive_query, /no_realize
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, nb = nb, bnames = bnames, data_type = dt
  res_dims = [ns, nl]

  ; prepare for clipping
  envi_delete_rois, /all  ; make sure no rois are defined
  envi_restore_rois, clip_roi
  roiids = envi_get_roi_ids(fid = fid, roi_names = attr_ids)
  if roiids[0] eq -1 then begin
    if ~keyword_set(verbose) then $
      void = error_message('ROI not suitable for this resolution: ' + string(mi.ps[0], format = '(i0)'), title = 'Sentinel L2A clipping', /error)
    return
  endif
  rids = envi_get_roi(roiids[0])
  xy = array_indices(res_dims, rids, /dim)
  high = max(xy, dim = 2, min = low)
  big_mask = make_array(ns, nl, /byte)
  big_mask[rids] = 1

  ns = high[0] - low[0] + 1
  nl = high[1] - low[1] + 1
  dims = [-1, low[0], high[0], low[1], high[1]]
  grid = make_array(nb, ns, nl, type = 12)  ; 12 = unsigned 16-bit
  mask = big_mask[low[0] : high[0], low[1] : high[1]]

  envi_convert_file_coordinates, fid, low[0], low[1], x, y, /to_map
  geokeys.ModelTiepointTag = [0, 0, 0, x, y, 0]

  for file = 0, n_elements(files) - 1 do begin
    envi_open_file, files[file], r_fid = fid, /no_interactive_query, /no_realize
    for b = 0, nb - 1 do begin
      data = envi_get_data(fid = fid, dims = dims, pos = b)
      grid[b, *, *] = (data * mask)
    endfor
    
    outname = getoutname(files[file], postfix = '_clip', ext = '.tif')
    outname = out_folder + path_sep() + file_basename(outname)
  
    write_tiff, outname, grid, /short, geotiff = geokeys
  
    envi_file_mng, id = fid, /remove  ; close the opened file again

  endfor

end

