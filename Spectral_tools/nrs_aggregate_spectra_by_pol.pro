;+
; :description:
;    Calculate an aggregated spectral profile for all polygons in the input features.
;    The output is a text table organised with one spectrum for each polygon in a column.
;
;
; :params:
;    shapefile : in, required
;      The input file with the polygon features
;    image : in, required
;      The input stack with the spectral information
;
; :keywords:
;    aggr_func : in, optional, default = average
;      The function to use for the aggregation; only average is implemented
;    xytable : in, optional, default = no
;      If true (yes) then the output will be an XY table with one polygon and spectrum
;      per row. If false (no) then the table will be organised with one polygon
;      and spectrum per column.
;    outname : out, optional
;      The name of the output table (CSV); if not specified will be
;      derived from the input image name
;    verbose : in, optional, default = true
;      If set (true) display all messages; if clear (false) skip display of all messages
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :author: nieuwenhuis
; :history:
;   - nov 2017 - WN, created
;-
pro nrs_aggregate_spectra_by_pol, shapefile, image $
                         , aggr_func = aggr_func $
                         , xytable = xytable $
                         , attribute = attribute $
                         , verbose = verbose $
                         , outname = outname $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  ; adjust verbose keyword for default (no keyword => true)
  verbose = keyword_set(verbose) || (n_elements(verbose) eq 0)

  if n_elements(aggr_func) eq 0 then aggr_func = 'mean'
  aggr_functions = ['min', 'max', 'mean', 'median']
  aggr_ix = where(strlowcase(aggr_func) eq aggr_functions, cnt)
  if cnt eq 0 then begin
    if keyword_set(verbose) then $
      void = error_message('Unsupported aggregation function')
    return
  endif

  ; determine if we deal with multiple images, or single image
  img_ext = nrs_get_file_extension(image)
  if img_ext eq '.lst' then begin
    all_images = nrs_read_listfile(image)
  endif else begin
    all_images = image
  endelse
  if n_elements(all_images) eq 0 then begin
    if keyword_set(verbose) then $
      void = error_message('No images found')
    return
  endif
  
  ext = nrs_get_file_extension(shapefile)
  if strlowcase(ext) ne '.shp' then begin
    if keyword_set(verbose) then $
      void = error_message('Shapefile expected; found: "' + file_basename(shapefile) + '"')
    return
  endif
  
  envi_delete_rois, /all  ; make sure no rois are defined
  nrs_read_shape_polygons, shapefile, polygons = pols, att_names = att_names

  minx = min((pols.bounds)[0, *])
  miny = min((pols.bounds)[1, *])
  maxx = max((pols.bounds)[4, *])
  maxy = max((pols.bounds)[5, *])
  hint_geo = ((abs(maxx) le 360.0 && abs(minx) le 360.0) $
    && (abs(maxy) le 360.0 && abs(miny) le 360.0))

  nr_pols = n_elements(pols.vertices)

  aggr = fltarr(nr_pols)
  att_names = strlowcase(att_names)
  attr_ix = where(strlowcase(attribute) eq att_names, ix_cnt)
  if ix_cnt eq 0 then attr_ix = 0 ; fallback: use first attribute if none specified
  
  nrs_set_progress_property, prog_obj, /start, title = 'Calculate aggregated polygon profile'
  
  more_than_one = (n_elements(all_images) gt 1)
  img_fids = []
  ns_all = -1
  nl_all = -1
  csy_all = -1
  nb = -1
  dt = -1
  bnames = []
  ; start opening all images and check dimensions
  for img = 0, n_elements(all_images) - 1 do begin
    
    cur_image = all_images[img]

    envi_open_file, cur_image, r_fid = mapID, /no_realize, /no_interactive_query
    envi_file_query, mapID, nb = nbloc, nl = nl, ns = ns, data_ignore_value = undef, data_type = dtloc
    mi = envi_get_map_info(fid = mapID, undefined = undef_csy)
    if img eq 0 then begin
      ns_all = ns
      nl_all = nl
      csy_all = mi
      nb = nbloc
      dt = dtloc
      if undef_csy eq 1 then begin
        if keyword_set(verbose) then $
          void = error_message('Spectral image has no coordinates')
        return
      endif
    endif else begin
      if (ns_all ne ns) || (nl_all ne nl) then begin
        if keyword_set(verbose) then $
          void = error_message('Images differ in spatial dimension')
        return
      endif
    endelse
    img_fids = [img_fids, mapID]
    bnames = [bnames, file_basename(cur_image)]
  endfor

  if n_elements(img_fids) eq 0 then begin
    if keyword_set(verbose) then $
      void = error_message('Spectral image has no coordinates')
    return
  endif
  
  cancelled = 0
  
  fid = img_fids[0]

  geo = envi_proj_create(/geographic)
  valid = bytarr(nr_pols)
  nr_prof = more_than_one ? n_elements(img_fids) : nb
  profiles = fltarr(nr_pols, nr_prof)
  pos = indgen(nb)
  polnames = []
  for p = 0, nr_pols - 1 do begin
    if nrs_update_progress(prog_obj, p, nr_pols, cancelled = cancelled) then break

    pol = *((pols.vertices)[p]) ; vertices of current polygon
    aval = (*(pols.attributes)[p]).(attr_ix) ; value of selected attribute of current polygon
    new_roi = envi_create_roi(ns = ns, nl = nl, name = string(aval))
    polnames = [polnames, aval]

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

    spectotal = []
    envi_define_roi, new_roi, /polygon, xpts = pixelX, ypts = pixelY
    if more_than_one then begin
      for b = 0, n_elements(img_fids) - 1 do begin
        spect = envi_get_roi_data(new_roi, fid = img_fids[b], pos = 0, addr = rix)
        if rix[0] eq -1 then begin
          break
        endif
        spectotal = [spectotal, spect]
      endfor
      spectotal = reform(spectotal,  n_elements(spect), n_elements(img_fids),/overwrite)
      spectotal = transpose(spectotal)
    endif else begin
      spectotal = envi_get_roi_data(new_roi, fid = fid, pos = pos, addr = rix)
    endelse
    if rix[0] eq -1 then begin
      continue    ; skip to next polygon
    endif

    valid[p] = 1
    
    ; now perform spatial aggregation; spectotal dim = nb * npixels 
    ag_fun = aggr_ix
    if n_elements(spectotal[0, *]) eq 1 then ag_fun = 99
    case ag_fun of
      0  : profiles[p, *] = min(spectotal, dim = 2)
      1  : profiles[p, *] = max(spectotal, dim = 2)
      2  : profiles[p, *] = mean(spectotal, dim = 2)
      3  : profiles[p, *] = median(spectotal, dim = 2)
      99 : profiles[p, *] = spectotal   ; single profile, just copy
    endcase
    
  endfor

  ; close all images
  for b = 0, n_elements(img_fids) - 1 do begin
    envi_file_mng, id = img_fids[b], /remove
  endfor
  
  ix = where(valid eq 1, cnt)
  if cnt eq 0 then begin
    if keyword_set(verbose) then $
      void = error_message('None of the polygons overlap image')
    return
  endif

  aval = polnames
  index = make_array(nr_pols, type = size(profiles, /type))
  index[*] = indgen(nr_pols) + 1
  if keyword_set(xytable) then begin
    hdr = ['Index', bnames]
    profiles = [transpose(index[ix]), transpose(profiles[ix, *])]
  endif else begin
    hdr = string(transpose(aval[ix]))
    profiles = profiles[ix, *]
  endelse
  
  if (n_elements(outname) eq 0) then outname = getOutname(image, postfix = '_prof', ext = '.csv')
  
  if cnt eq 1 then profiles = reform(profiles, n_elements(hdr), 1, /overwrite)
  write_csv, outname, header = hdr, profiles

end
