pro change_detection_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_inputImage')
  widget_control, fld, get_value = inputfile
  len = strlen(strtrim(inputfile))
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_year_panel')
  widget_control, fld, sensitiv = len gt 0
  fld = widget_info(event.top, find_by_uname = 'change_detection_time_panel')
  widget_control, fld, sensitiv = len gt 0
  
  if strlen(strtrim(inputfile)) eq 0 then return
  
  outfile = getOutname(inputfile, postfix = '_det', ext = '.')
  cntfile = getOutname(inputfile, postfix = '_cnt', ext = '.')
  magfile = getOutname(inputfile, postfix = '_mag', ext = '.')

  envi_open_file, inputfile, r_fid = fid, /no_realize, /no_interactive_query
  envi_file_query, fid, nb = nb
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(strtrim(ndvilayers))
  change_detection_update_time_fields, event, nb, ndvi_py
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_outputImage')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = outfile
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_outputCounting')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = cntfile

  fld = widget_info(event.top, find_by_uname = 'change_detection_magnitude')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = magfile
end

pro change_detection_handle_outputfile, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_outputImage')
  widget_control, fld, get_value = outputfile
  
  if strlen(strtrim(outputfile)) eq 0 then return
  
  cntfile = getOutname(outputfile, postfix = '_cnt', ext = '.')
  magfile = getOutname(outputfile, postfix = '_mag', ext = '.')

  fld = widget_info(event.top, find_by_uname = 'change_detection_outputCounting')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = cntfile

  fld = widget_info(event.top, find_by_uname = 'change_detection_magnitude')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = magfile
end

function change_detection_handle_NPY_change, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_inputImage')
  widget_control, fld, get_value = inputfile
  
  nb = -1
  if strlen(strtrim(inputfile)) gt 0 then begin
    envi_open_file, inputfile, r_fid = fid, /no_realize, /no_interactive_query
    envi_file_query, fid, nb = nb
  endif
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(ndvilayers)
  
  void = change_detection_handle_count(event)
  
  change_detection_update_time_fields, event, nb, ndvi_py
end

function change_detection_handle_time_ft, event
  fld_f = widget_info(event.top, find_by_uname = 'change_detection_time_from')
  widget_control, fld_f, get_uvalue = from
  widget_control, fld_f, get_value = user_from
  fld = widget_info(event.top, find_by_uname = 'change_detection_time_to')
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

  void = change_detection_handle_count(event)
end

function change_detection_handle_count, event
  fld = widget_info(event.top, find_by_uname = 'change_detection_countingCount')
  widget_control, fld, get_value = perc
  fld_f = widget_info(event.top, find_by_uname = 'change_detection_time_from')
  widget_control, fld_f, get_value = user_from
  fld = widget_info(event.top, find_by_uname = 'change_detection_time_to')
  widget_control, fld, get_value = user_to

  perc = fix(perc)
  from = fix(user_from[0])
  to = fix(user_to[0])
  
  count = ceil(perc * (to - from +1) / 100.0)

  fld = widget_info(event.top, find_by_uname = 'cd_convert_label')
  widget_control, fld, set_value = string(count, format = '("(", i0, " bands)")')
end

pro change_detection_update_time_fields, event, nb, ndvi_py
  if nb gt 0 then begin
    nryears = nb / ndvi_py
    ystr = string(indgen(nryears) + 1, format = '(i0)')
    
    fld = widget_info(event.top, find_by_uname = 'change_detection_years_combo')
    widget_control, fld, set_value = [ 'All', ystr]
    widget_control, fld, set_combobox_select = 0
  endif
  
  fld_f = widget_info(event.top, find_by_uname = 'change_detection_time_from')
  widget_control, fld_f, get_uvalue = from
  fromobj = (from.Object)->getID()
  widget_control, fromobj, get_uvalue = user_from
  fld = widget_info(event.top, find_by_uname = 'change_detection_time_to')
  widget_control, fld, get_uvalue = to
  toobj = (to.Object)->getID()
  widget_control, toobj, get_uvalue = user_to
  widget_control, fld_f, set_value = string(user_to < user_from, format = '(i0)')
  widget_control, fld, set_value = string(user_to < ndvi_py, format = '(i0)')
end

pro change_detection_toggle_counting, event
  if (event.id eq widget_info(event.top, find_by_uname='change_detection_counting')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility of the sequence output panel visible
    cpanel = widget_info(event.top, find_by_uname='change_detection_countingDetailPanel')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro change_detection_toggle_magcounting, event
  if (event.id eq widget_info(event.top, find_by_uname='change_detection_mag4count')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility of the sequence output panel visible
    cpanel = widget_info(event.top, find_by_uname='change_detection_mag_panel')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro change_detection_handleGo, event
  ; collect parameters
  fld = widget_info(event.top, find_by_uname = 'change_detection_inputImage')
  widget_control, fld, get_value = inputfile

  fld = widget_info(event.top, find_by_uname = 'change_detection_ndvipy')
  widget_control, fld, get_value = ndvilayers

  fld = widget_info(event.top, find_by_uname = 'change_detection_years_combo')
  sel_year = widget_info(fld, /combobox_gettext)

  fld = widget_info(event.top, find_by_uname = 'change_detection_time_from')
  widget_control, fld, get_value = fromtime
    
  fld = widget_info(event.top, find_by_uname = 'change_detection_time_to')
  widget_control, fld, get_value = totime

  fld = widget_info(event.top, find_by_uname = 'change_detection_segments')
  widget_control, fld, get_value = segments
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_classes')
  widget_control, fld, get_value = classfile

  fld = widget_info(event.top, find_by_uname='change_detection_sdTable')
  widget_control, fld, get_value = sdtablefile
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_mask')
  widget_control, fld, get_value = maskname
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_outputImage')
  widget_control, fld, get_value = outfile
  
  fld = widget_info(event.top, find_by_uname='change_detection_counting')
  doCnt = widget_info(fld, /button_set)
  
  fld = widget_info(event.top, find_by_uname='change_detection_mag4count')
  doMag = widget_info(fld, /button_set)
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_countingCount')
  widget_control, fld, get_value = cntcount_str
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_outputCounting')
  widget_control, fld, get_value = cntname

  fld = widget_info(event.top, find_by_uname = 'change_detection_magnitude')
  widget_control, fld, get_value = magname
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_mag_aggr_combo')
  aggr_method = widget_info(fld, /combobox_gettext)
  
  ; now check all inputs
  if strlen(strtrim(inputfile)) eq 0 then begin
    void = error_message('Missing input stack', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(classfile)) eq 0 then begin
    void = error_message('Missing class image', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(sdtablefile)) eq 0 then begin
    void = error_message('Missing input stdevtable', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(outfile)) eq 0 then begin
    void = error_message('Missing output name', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  
  ndvi_py = fix(strtrim(ndvilayers[0]))
  if (ndvi_py lt 10) or (ndvi_py gt 36) then begin
    void = error_message('Number of NDVI layers expected between 10 and 36', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif

  if sel_year eq 'All' then sel_year = -1 else sel_year = fix(sel_year[0])
  totime = fix(totime[0])
  fromtime = fix(fromtime[0])

  useSeg = strlen(strtrim(segments)) ne 0
  useMask = strlen(strtrim(maskname)) ne 0
  doMag = doMag * doCnt

  cntcount = fix(cntcount_str[0])  ; cntcount in 100 x percentage
  if doCnt then $
    if (strlen(strtrim(cntname)) eq 0) or (cntcount eq 0) then begin
      void = error_message('Counting output not specified', title = 'Change detection error', /error, /noname, traceback = 0)
      return
    endif
  cntcount = cntcount < 100.0
  
  if doMag then $
    if (strlen(strtrim(magname)) eq 0) then begin
      void = error_message('Probability of change output not specified', title = 'Change detection error', /error, /noname, traceback = 0)
      return
    endif

  ; open inputs
  envi_open_file, inputfile, r_fid = ndvi, /no_realize, /no_interactive_query
  if ndvi eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass
  
  envi_file_query, ndvi, dims = dims, ns = ns, nl = nl, nb = nrlayers 
  mi_ref = envi_get_map_info(fid = ndvi, undefined = undef_csy)

  lut = read_csv(sdtablefile, header = header)
  sd_tab = lut.(1)
  
  seg_id = -1
  if useSeg then begin
    envi_open_file, segments, r_fid = seg_id, /no_realize, /no_interactive_query
    nrs_load_class_image, seg_id, cldata = segdata, num_classes = nrsegs
  endif
    
  if useMask then $
    envi_open_file, maskname, r_fid = m_fid, /no_realize, /no_interactive_query

  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Detect NDVI changes")
  progressBar -> Start

  ndvi_py_org = ndvi_py
  nb = nrlayers
  nryears = nrlayers / ndvi_py
  
  ndvi_py = totime - fromtime + 1
  if sel_year eq -1 then sel_year = 1 else nryears = 1
  sel_year -= 1
  nrlayers = ndvi_py * nryears
  
  ;  allocate memory for output
  outdata = bytarr(ns * nl, nrlayers)
  cntdata = bytarr(ns * nl, nryears)
  magdata = fltarr(ns * nl, nryears)

  ; allocate memory for loop
  yearmag = fltarr(ns * nl, ndvi_py)

  diff = fltarr(ns * nl)
  ano_layer = bytarr(ns * nl)
  mag_layer = fltarr(ns * nl)
  cntcount = ceil(cntcount * ndvi_py / 100.0) ; convert to actual bands from percentage
  ; base calculation: find differences with averages per class
  for y = 0, nryears - 1 do begin
    yearmag[*] = 0.0
    ; calculate anomalies and magnitudes for an entire year
    for lpy = 0, ndvi_py - 1 do begin
      layer = y * ndvi_py + lpy
      inp_layer = (y + sel_year) * ndvi_py_org + (lpy + fromtime - 1)
      if nrs_update_progress(progressBar, layer, nrlayers, cancelled = cancelled) then return

      ldata = envi_get_data(fid = ndvi, dims = dims, pos = inp_layer)
  
      if useSeg then begin
        nrs_ndvi_diff_from_avg, ldata, segdata, diffout = diff
      endif else begin
        nrs_ndvi_diff_from_avg, ldata, cldata, diffout = diff
      endelse
      
      nrs_ndvi_sd_lookup, diff, cldata, nrclass, sd_tab, ndvi_py, lpy $
                        , outdata = ano_layer, magdata = mag_layer
      outdata[*, layer] = ano_layer
      yearmag[*, lpy] = mag_layer
    endfor
  
    ; indicate yearly anomaly in case more than a minimum count of anomalies reached
    for p = 0L, ns * nl - 1L do begin
      b = y * ndvi_py
      ix = where(outdata[p, b : b + ndvi_py - 1] gt 0, count)
      cntdata[p, y] = count ge cntcount
      if count ge cntcount then begin
        mag_ = yearmag[p, ix]
        case aggr_method of
          'sum' : magdata[p, y] = total(mag_)
          'avg' : magdata[p, y] = total(mag_) / count
          'min' : magdata[p, y] = min(mag_)
          'max' : magdata[p, y] = max(mag_)
        endcase
      endif
    endfor
  endfor
    
  ; now apply masking if specified
  if useMask then begin
    md = envi_get_data(fid = m_fid, dims = dims, pos = 0)
    ix = where(md eq 0, count)
    if count gt 0 then begin
      outdata[ix, *] = 0
      cntdata[ix, *] = 0
      magdata[ix, *] = 0.0
    endif
  endif
  outdata = reform(outdata, ns, nl, nrlayers, /overwrite)
  cntdata = reform(cntdata, ns, nl, nryears, /overwrite)
  magdata = reform(magdata, ns, nl, nryears, /overwrite)

  out_det = getoutname(outfile, postfix = '', ext = '.')  ; remove extension
  if doCnt then out_cnt = getoutname(cntname, postfix = '', ext = '.')  ; remove extension
  if doMag then out_mag = getoutname(magname, postfix = '', ext = '.')  ; remove extension

  ; build band names
  nn = indgen(nrlayers)
  dnames = 'Year.nr ' + string(sel_year + 1 + nn / ndvi_py, format = '(I0)') + '.' + string(fromtime + nn mod ndvi_py, format = '(I0)')
  bc = indgen(nryears) + sel_year + 1
  bnames = string(bc, format = '("Year ",I02)')
  mnames = string(bc, format = '("Magnitude (year ",I02,")")')

  if undef_csy eq 1 then begin
    envi_write_envi_file, outdata, out_name = out_det, bnames = dnames
    if doCnt then envi_write_envi_file, cntdata, out_name = out_cnt, bnames = bnames
    if doMag then envi_write_envi_file, magdata, out_name = out_mag, bnames = mnames
  endif else begin
    envi_write_envi_file, outdata, out_name = out_det, bnames = dnames, map_info = mi_ref
    if doCnt then envi_write_envi_file, cntdata, out_name = out_cnt, bnames = bnames, map_info = mi_ref
    if doMag then envi_write_envi_file, magdata, out_name = out_mag, bnames = mnames, map_info = mi_ref
  endelse

  progressBar -> Destroy
end