pro change_detection_v2_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_inputImage')
  widget_control, fld, get_value = inputfile
  len = strlen(strtrim(inputfile))
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_year_panel')
  widget_control, fld, sensitiv = len gt 0
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_time_panel')
  widget_control, fld, sensitiv = len gt 0
  
  if strlen(strtrim(inputfile)) eq 0 then return
  
  magfile = getOutname(inputfile, postfix = '_mag', ext = '.')

  envi_open_file, inputfile, r_fid = fid, /no_realize, /no_interactive_query
  envi_file_query, fid, nb = nb
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(strtrim(ndvilayers))
  change_detection_v2_update_time_fields, event, nb, ndvi_py
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_magnitude')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = magfile
end

pro change_detection_v2_handle_NPY_change, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_inputImage')
  widget_control, fld, get_value = inputfile
  
  nb = -1
  if strlen(strtrim(inputfile)) gt 0 then begin
    envi_open_file, inputfile, r_fid = fid, /no_realize, /no_interactive_query
    envi_file_query, fid, nb = nb
  endif
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(ndvilayers)
  
  change_detection_v2_update_time_fields, event, nb, ndvi_py
end

function change_detection_v2_handle_time_ft, event
  fld_f = widget_info(event.top, find_by_uname = 'change_detection_v2_time_from')
  widget_control, fld_f, get_uvalue = from
  widget_control, fld_f, get_value = user_from
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_time_to')
  widget_control, fld, get_uvalue = to
  widget_control, fld, get_value = user_to
  fromobj = (from.Object)->getID()
  toObj = (to.Object)->getID()
  
  if event.id eq fromobj then begin
    widget_control, fromobj, set_uvalue = fix(user_from)
  endif
  if event.id eq toObj then begin
    widget_control, toObj, set_uvalue = fix(user_to)
  endif
end

pro change_detection_v2_update_time_fields, event, nb, ndvi_py
  if nb gt 0 then begin
    nryears = nb / ndvi_py
    ystr = string(indgen(nryears) + 1, format = '(i0)')
    
    fld = widget_info(event.top, find_by_uname = 'change_detection_v2_years_combo')
    widget_control, fld, set_value = [ 'All', ystr]
    widget_control, fld, set_combobox_select = 0
  endif

  fld_f = widget_info(event.top, find_by_uname = 'change_detection_v2_time_from')
  widget_control, fld_f, get_uvalue = from
  fromobj = (from.Object)->getID()
  widget_control, fromobj, get_uvalue = user_from
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_time_to')
  widget_control, fld, get_uvalue = to
  toobj = (to.Object)->getID()
  widget_control, toobj, get_uvalue = user_to
  widget_control, fld_f, set_value = string(user_to < user_from, format = '(i0)')
  widget_control, fld, set_value = string(user_to < ndvi_py, format = '(i0)')
end

pro change_detection_v2_handleGo, event
  compile_opt idl2, logical_predicate

  ; collect parameters
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_inputImage')
  widget_control, fld, get_value = inputfile

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_ndvipy')
  widget_control, fld, get_value = ndvilayers

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_years_combo')
  sel_year = widget_info(fld, /combobox_gettext)

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_time_from')
  widget_control, fld, get_value = fromtime
    
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_time_to')
  widget_control, fld, get_value = totime

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_classes')
  widget_control, fld, get_value = classfile

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_refimage')
  widget_control, fld, get_value = refimage

  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_magnitude')
  widget_control, fld, get_value = magname
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_sd_muly_combo')
  sd_mult_str = widget_info(fld, /combobox_gettext)
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_mask')
  widget_control, fld, get_value = pixmask_str
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_v2_absdiff')
  abs_diff = widget_info(fld, /button_set)
  
  
  ; now check all inputs
  if strlen(strtrim(inputfile)) eq 0 then begin
    void = error_message('Missing NDVI (reference period)', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(classfile)) eq 0 then begin
    void = error_message('Missing classified image', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(refimage)) eq 0 then begin
    void = error_message('Missing NDVI (initial period)', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  
  ndvi_py = fix(strtrim(ndvilayers[0]))
;  if (ndvi_py lt 10) or (ndvi_py gt 36) then begin
;    void = error_message('Number of NDVI layers expected between 10 and 36', title = 'Change detection error', /error, /noname, traceback = 0)
;    return
;  endif
  sd_mult = float(strtrim(sd_mult_str[0]))

  pixmask = fix(strtrim(pixmask_str[0]))
  if pixmask lt 0 then begin
    void = error_message('Number of pixels cannot be negative', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif

  if sel_year eq 'All' then sel_year = -1 else sel_year = fix(sel_year[0])
  totime = fix(totime[0])
  fromtime = fix(fromtime[0])

  if (strlen(strtrim(magname)) eq 0) then begin
    void = error_message('Probability of change output not specified', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  
  ; initialise tranquilizer
  progressBar = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Detect NDVI changes" $
    , /fast_loop $
    )

  nrs_change_detection_v2_exec, inputfile, refimage, classfile, ndvi_py, sd_mult, pixmask $
                              , fromtime, totime, sel_year $
                              , abs_diff $
                              , magname $
                              , prog_obj = progressBar, cancelled = cancelled
                              
  if obj_valid(progressBar) then progressBar -> Destroy

end

pro nrs_change_detection_v2_exec, inputfile, refimage, classfile, ndvi_py, sd_mult, pixmask $
                                , fromtime, totime, sel_year $
                                , abs_diff $
                                , magname $
                                , interdiff = interdiff $
                                , prog_obj = progressBar, cancelled = cancelled

  compile_opt idl2, logical_predicate

  cancelled = 1
  
  interdiff = keyword_set(interdiff)  
  nodata = -9999.0

  ; open inputs
  envi_open_file, inputfile, r_fid = ndvi, /no_realize, /no_interactive_query
  if ndvi eq -1 then return

  envi_open_file, refimage, r_fid = ndvi_ref, /no_realize, /no_interactive_query
  if ndvi_ref eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass
  
  envi_file_query, ndvi, dims = dims, ns = ns, nl = nl, nb = nrlayers 
  mi_ref = envi_get_map_info(fid = ndvi, undefined = undef_csy)
  if undef_csy then void = temporary(mi_ref)

  envi_file_query, ndvi_ref, ns = ns_ref, nl = nl_ref 
  envi_file_query, class, ns = ns_class, nl = nl_class
  
  dim_ok = (ns eq ns_ref) && (ns eq ns_class) && (nl eq nl_ref) && (nl eq nl_class)
  if dim_ok eq 0 then begin
    void = dialog_message('Check images: dimensions are not the same' $
                          , title = 'Probability of change' $
                          , /error)
    return  
  endif 

  cancelled = 0
  
  nrs_set_progress_property, progressBar, /start, title = 'Detect NDVI changes'

  ndvi_py_org = ndvi_py
  nb = nrlayers
  nryears = nrlayers / ndvi_py
  
  ndvi_py = totime - fromtime + 1
  if sel_year eq -1 then sel_year = 1 else nryears = 1
  sel_year -= 1
  nrlayers = ndvi_py * nryears
  
  ;  allocate memory for output
  magdata = fltarr(ns * nl, nryears)

  ; allocate memory for loop
  yearmag = fltarr(ns * nl, ndvi_py)

  mag_layer = fltarr(ns * nl)

  ; create segments, and the mask to remove small areas
  nrs_set_progress_property, progressBar, title = 'Finding segments'
  nrs_area_numbering_data, cldata, areas = segdata, undef = undef, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  ix = where(segdata eq undef, count)
  nrs_set_progress_property, progressBar, /start, title = 'Build mask'
  nrs_build_mask_by_area, segdata, area = pixmask, mask = mask, undef = undef, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  mask = reform(mask, ns, nl, /overwrite)
  
  ; calculate the SD LUT from the historical data
  nrs_set_progress_property, progressBar, /start, title = 'Calculate pooled LUT'
  nrs_calc_class_stddev, ndvi_ref, class, stdevs = stdevs, prog_obj = progressBar, cancelled = cancelled
  if cancelled eq 1 then return
  
  nrs_calc_pooled_sd, stdevs, ndvi_py, poolsd = poolsd
  
  lut = poolsd * sd_mult
  
  if interdiff eq 1 then begin
    inter_name = getoutname(magname, postfix = '_pp', ext = '.dat')
    openw, unit, inter_name, /get_lun 
  endif

  mask_count = 0
  mask_ix = [] 
  ; prepare mask, if needed
  if pixmask gt 0 then begin
    mask_ix = where(mask eq 0, mask_count)
  endif

  ; base calculation: find differences with averages per class
  nrs_set_progress_property, progressBar, /start, title = 'Detect NDVI changes'
  for y = 0, nryears - 1 do begin
    yearmag[*] = 0.0  ; make sure we don't change the type
    ; calculate anomalies and magnitudes for an entire year
    for lpy = 0, ndvi_py - 1 do begin
      layer = y * ndvi_py + lpy
      inp_layer = (y + sel_year) * ndvi_py_org + (lpy + fromtime - 1)
      if nrs_update_progress(progressBar, layer, nrlayers) then begin
        return
      endif
      ldata = envi_get_data(fid = ndvi, dims = dims, pos = inp_layer)
  
      nrs_ndvi_magdata, ldata, cldata, segdata, lut, ndvi_py, lpy, magdata = mag_layer, abs_diff = abs_diff
      
      yearmag[*, lpy] = mag_layer

      if interdiff eq 1 then begin
        if mask_count gt 0 then begin
          mag_layer[mask_ix] = nodata
        endif
        writeu, unit, mag_layer
      endif
      
    endfor
  
    ; indicate yearly (period) anomaly
    for p = 0L, ns * nl - 1L do begin
      magdata[p, y] = total(yearmag[p, *])
    endfor
    
  endfor
    
  if mask_count gt 0 then begin
    magdata[mask_ix, *] = nodata
  endif

  magdata = reform(magdata, ns, nl, nryears, /overwrite)

  out_mag = getoutname(magname, postfix = '', ext = '.dat')

  ; build band names
  nn = indgen(nrlayers)
  dnames = 'Year.nr ' + string(sel_year + 1 + nn / ndvi_py, format = '(I0)') + '.' + string(fromtime + nn mod ndvi_py, format = '(I0)')
  bc = indgen(nryears) + sel_year + 1
  mnames = string(bc, format = '("Magnitude (Year ",I02,")")')

  envi_write_envi_file, magdata, out_name = out_mag, bnames = mnames, map_info = mi_ref $
                      , data_ignore_value = nodata

  if interdiff eq 1 then begin
    ; Determine band names
    nb_out = ndvi_py * nryears
    yy = indgen(nb_out) / ndvi_py + 1 
    band = indgen(nb_out) mod ndvi_py + 1
    bnames = string([transpose(yy), transpose(band)], format = '("Magnitude (Year.Band ",i02, ".", i02, ")")')
    
    envi_setup_head, fname = inter_name $
            , data_type = 4 $   ; float
            , /write $
            , interleave = 0 $  ; BSQ
            , nb = nb_out, nl = nl, ns = ns $
            , bnames = bnames $
            , map_info = mi
  
    close, unit
    free_lun, unit 
  endif
end