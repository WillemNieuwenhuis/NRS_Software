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
      if multiple then $
        print, 'No Sentinel L2A product found' $
      else $
        print, 'No Sentinel L2A product found in: ' + tile_folder

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
;    Create ENVI stacks for the 10m and 20m atmospherically corrected Sentinel2 bands (L2A product).
;    The 10m stack contains: B2, B3, B4, B8, in that order
;    The 20m stack contains: B2, B3, B4, B5, B6, B7, B8A, B11, B12, in that order.
;    When include_band8 is set, the 20m stack contains: B2, B3, B4, B5, B6, B7, B8, B8A, B11, B12, in that order.
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
;    include_band8 : in, optional, default = no
;      Include band 8 (NIR, 10m) resampled to 20m also in the 20m stack. Widebanded band 8 spectrally overlaps
;      band 7 and band 8A which both have small bandwidth, so may not always be needed. (Not implemented yet)
;
; :Author: nieuwenhuis

; :History:
;   - august 2017: created
;-
pro nrs_convert_S2ENVI_to_ENVIstack, folders, out_folder, include_band8 = include_band8
  compile_opt idl2, logical_predicate

  for fold = 0, n_elements(folders) - 1 do begin
    
    folder = folders[fold]

    ; find tile data folder and extract meta data
    tile_folder = file_search(count = fc, folder + '\granule\s2*')    ; old structure
    if fc eq 0 then tile_folder = file_search(folder + '\granule\l2*')  ; new structure
  
    imgfolder = [tile_folder, tile_folder, tile_folder] + ['\IMG_DATA\R10m', '\IMG_DATA\R20m', '\IMG_DATA\R60m']
    res_str = ['R10m', 'R20m', 'R60m']
  
    pattern = '_B'  ; only select the regular spectral bands
    folder10 = imgfolder[0]
    folder20 = imgfolder[1]
    files_10m = nrs_find_images(folder10, pattern, ext = 'jp2')
    files_20m = nrs_find_images(folder20, pattern, ext = 'jp2')
  
    nrfiles = n_elements(files_10m)
    bpos = strpos(files_10m, '_B')
    bnam = strmid(reform(files_10m, 1, nrfiles), reform(bpos + 2, 1, nrfiles), 2)
    six = sort(fix(bnam))
    files_10m = files_10m[six] ; put the bands in correct order
  
    nrfiles = n_elements(files_20m)
    bpos = strpos(files_20m, '_B')
    bnam = strmid(reform(files_20m, 1, nrfiles), reform(bpos + 2, 1, nrfiles), 2)
    six = sort(fix(bnam))
    files_20m = files_20m[six] ; put the bands in correct order
  
    ; prepare arrays for layer stacking
    bnames_10m = ['02', '03', '04', '08']
    bnames_20m = ['02', '03', '04', '05', '06', '07', '8A', '11', '12']
  ;  if keyword_set(include_band8) then bnames_20m = ['02', '03', '04', '05', '06', '07', '8', '8A', '11', '12']
  
    ; stack 10m bands
    name = file_basename(files_10m[0], '.jp2')
    ix = strpos(name, 'B02')
    name = strmid(name, 0, ix) + 'stack' + strmid(name, ix + 3)
    outname = out_folder + path_sep() + name + '.dat'
    fids = []
    for f = 0, n_elements(files_10m) - 1 do begin
      envi_open_file, files_10m[f], r_fid = fid_10m, /no_interactive_query, /no_realize
      if f eq 0 then begin
        envi_file_query, fid_10m, dims = dims_10m, data_type = dt
        mi = envi_get_map_info(fid = fid_10m)
      endif
  
      fids = [fids, fid_10m]
    endfor
  
    all_dims = rebin(dims_10m, 5, n_elements(fids))
    pos = intarr(n_elements(fids))
    envi_doit, 'envi_layer_stacking_doit', fid = fids, pos = pos $
      , out_name = outname $
      , dims = all_dims $
      , out_dt = dt, out_bname = bnames_10m $
      , out_proj = mi.proj, out_ps = mi.ps $
      , /invisible, /no_realize
  
    for fi = 0, n_elements(fids) - 1 do $
      envi_file_mng, id = fids[fi], /remove
  
    ; stack 20m bands
    name = file_basename(files_20m[0], '.jp2')
    ix = strpos(name, 'B02')
    name = strmid(name, 0, ix) + 'stack' + strmid(name, ix + 3)
    outname = out_folder + path_sep() + name + '.dat'
    fids = []
    for f = 0, n_elements(files_20m) - 1 do begin
      envi_open_file, files_20m[f], r_fid = fid_20m, /no_interactive_query, /no_realize
      if f eq 0 then begin
        envi_file_query, fid_20m, dims = dims_20m, data_type = dt
        mi = envi_get_map_info(fid = fid_20m)
      endif
  
      fids = [fids, fid_20m]
    endfor
  
    all_dims = rebin(dims_20m, 5, n_elements(fids))
    pos = intarr(n_elements(fids))
    envi_doit, 'envi_layer_stacking_doit', fid = fids, pos = pos $
      , out_name = outname $
      , dims = all_dims $
      , out_dt = dt, out_bname = bnames_20m $
      , out_proj = mi.proj, out_ps = mi.ps $
      , /invisible, /no_realize
  
    for fi = 0, n_elements(fids) - 1 do $
      envi_file_mng, id = fids[fi], /remove

  endfor

end

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

