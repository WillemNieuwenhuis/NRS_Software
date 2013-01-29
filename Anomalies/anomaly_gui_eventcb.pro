pro anomaly_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'anomaly_inputImage')
  widget_control, fld, get_value = file
  
  if strlen(strtrim(file)) eq 0 then return
  
  outname = getoutname(file, ext = '.')
  seqname = getoutname(file, postfix = '_aggr', ext = '.')
  cntname = getoutname(file, postfix = '_cnt', ext = '.')
  seasonname = getoutname(file, postfix = '_seasons', ext = '.')
  magname = getoutname(file, postfix = '_mag', ext = '.')
  
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputImage')
  widget_control, fld, set_value = outname
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputSequence')
  widget_control, fld, set_value = seqname
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputCounting')
  widget_control, fld, set_value = cntname
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputSeasons')
  widget_control, fld, set_value = seasonname
  fld = widget_info(event.top, find_by_uname = 'anomaly_mag_output')
  widget_control, fld, set_value = magname
  fld = widget_info(event.top, find_by_uname = 'anomaly_magnitude')
  widget_control, fld, set_value = magname
end

pro anomaly_handle_outputfile, event
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputImage')
  widget_control, fld, get_value = outname

  fld_seq = widget_info(event.top, find_by_uname = 'anomaly_outputSequence')
  postfix_seq = '_aggr'
  fld_cnt = widget_info(event.top, find_by_uname = 'anomaly_outputCounting')
  postfix_cnt = '_cnt'
  fld_season = widget_info(event.top, find_by_uname = 'anomaly_outputSeasons')
  postfix_season = '_seasons'
  
  widget_control, fld_seq, get_value = seqname
  if strlen(strtrim(seqname)) gt 0 then return
  
  name = getoutname(outname, postfix = postfix_seq, ext = '.')
  widget_control, fld_seq, set_value = name
  
  name = getoutname(outname, postfix = postfix_cnt, ext = '.')
  widget_control, fld_cnt, set_value = name
  
  name = getoutname(outname, postfix = postfix_season, ext = '.')
  widget_control, fld_season, set_value = name
end

pro anomaly_toggle_aggregate, event
  if (event.id eq widget_info(event.top, FIND_BY_UNAME='anomaly_aggregate')) then begin
    isOn = widget_info(event.id, /BUTTON_SET)
    ; toggle visibility of the sequence output panel visible
    cpanel = widget_info(event.top, FIND_BY_UNAME='anomaly_sequencePanel')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro anomaly_toggle_counting, event
  if (event.id eq widget_info(event.top, find_by_uname='anomaly_counting')) then begin
    anomaly_update_sequence_page, event
  endif
end

pro anomaly_toggle_magcounting, event
  if (event.id eq widget_info(event.top, find_by_uname='anomaly_mag4count')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility of the sequence output panel visible
    cpanel = widget_info(event.top, find_by_uname='anomaly_mag_panel')
    widget_control, cpanel, sensitive = isOn
    fld = widget_info(event.top, find_by_uname='anomaly_sdTable')
    widget_control, fld, sensitive = isOn
  endif
end

pro anomaly_update_sequence_page, event
  fld = widget_info(event.top, find_by_uname='anomaly_counting')
  isOn = widget_info(fld, /button_set)
  ; toggle visibility of the sequence output panel visible
  cpanel = widget_info(event.top, find_by_uname='anomaly_countingDetailPanel')
  widget_control, cpanel, sensitive = isOn
  ; also adjust visibility of the range table if needed
  fld = widget_info(event.top, find_by_uname='anomaly_inputTable')
  widget_control, fld, sensitive = 1
  ; also adjust visibility of the sd table if needed
  fld = widget_info(event.top, find_by_uname='anomaly_mag4count')
  isMagOn = widget_info(fld, /button_set)
  fld = widget_info(event.top, find_by_uname='anomaly_sdTable')
  widget_control, fld, sensitive = isOn * isMagOn
end

pro anomaly_update_seasonal_page, event
  fld = widget_info(event.top, find_by_uname='anomaly_inputTable')
  widget_control, fld, sensitive = 1
  fld = widget_info(event.top, find_by_uname='anomaly_sdTable')
  widget_control, fld, sensitive = 0
end

pro anomaly_update_change_detect_page, event
  fld = widget_info(event.top, find_by_uname='anomaly_inputTable')
  widget_control, fld, sensitive = 0
  fld = widget_info(event.top, find_by_uname='anomaly_sdTable')
  widget_control, fld, sensitive = 1
end
       
pro anomaly_handle_tabchange, event
  fld = widget_info(event.top, find_by_uname = 'anomaly_scenarioTabs')
  curPage = widget_info(fld, /tab_current)
  case curPage of
    0: anomaly_update_sequence_page, event
    1: anomaly_update_seasonal_page, event
    2: anomaly_update_change_detect_page, event
  endcase
end

function anomaly_calc_anomalies, inputfile, classfile, tablefile, ndvi_py, outfile, output, undefined
  ; open the input image and the class file 
  envi_open_file, inputfile, r_fid = fid, /no_interactive_query, /no_realize
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  envi_file_query, fid, nb = nrlayers

  envi_open_file, classfile, r_fid = fid_cl, /no_interactive_query, /no_realize
  
  ; Now start calculations
  nrs_detect_anomalies, fid_cl, fid, tablefile, output, bnames = bnames, ndvi_py = ndvi_py
  if n_elements(output) eq 1 then return, 1 ; user stopped calculation

  ; store the result
  outname = getoutname(outfile, postfix = '', ext = '.')  ; remove extension
  if n_elements(bnames) le 1 then begin
    bc = indgen(nrlayers)
    bnames = string(bc, format = '("Band ",I02)')
  endif
  
  if undefined eq 1 then $
    envi_write_envi_file, output, out_name = outname, bnames = bnames  $
  else $
    envi_write_envi_file, output, out_name = outname, bnames = bnames, map_info = mi
  
  return, 0 ; calculation finished successfully
end

function anomaly_calc_changes, inputfile, classfile, tablefile, ndvi_py, outfile, output, undefined
  ; open the input image and the class file 
  envi_open_file, inputfile, r_fid = fid, /no_interactive_query, /no_realize
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  envi_file_query, fid, nb = nrlayers

  envi_open_file, classfile, r_fid = fid_cl, /no_interactive_query, /no_realize
  
  ; Now start calculations
  nrs_ndvi_change_detection, fid_cl, fid, tablefile, output, bnames = bnames, ndvi_py = ndvi_py
  if n_elements(output) eq 1 then return, 1 ; user stopped calculation

  ; store the result
  outname = getoutname(outfile, postfix = '', ext = '.')  ; remove extension
  if n_elements(bnames) le 1 then begin
    bc = indgen(nrlayers)
    bnames = string(bc, format = '("Band ",I02)')
  endif
  
  if undefined eq 1 then $
    envi_write_envi_file, output, out_name = outname, bnames = bnames  $
  else $
    envi_write_envi_file, output, out_name = outname, bnames = bnames, map_info = mi
  
  return, 0 ; calculation finished successfully
end

function anomaly_get_inout, event
  ; collect parameters
  fld = widget_info(event.top, find_by_uname = 'anomaly_inputImage')
  widget_control, fld, get_value = inputfile

  fld = widget_info(event.top, find_by_uname = 'anomaly_classes')
  widget_control, fld, get_value = classfile

  fld = widget_info(event.top, find_by_uname = 'anomaly_inputTable')
  widget_control, fld, get_value = rangetablefile
  
  fld = widget_info(event.top, find_by_uname='anomaly_sdTable')
  widget_control, fld, get_value = sdtablefile
  
  onlyAggr = (strlen(strtrim(inputfile)) * strlen(strtrim(classfile)) eq 0) 

  fld = widget_info(event.top, find_by_uname = 'anomaly_outputImage')
  widget_control, fld, get_value = outfile
  if strlen(strtrim(outfile)) eq 0 then return, 0
  
  struc = {infile:strtrim(inputfile), classfile:strtrim(classfile), $
           tblfile:strtrim(rangetablefile), $
           sdtblfile:strtrim(sdtablefile), $
           outfile:strtrim(outfile), onlyAggr:onlyAggr}
           
  return, struc
end

function anomaly_load_output, outfile, undefined
  envi_open_file, outfile, r_fid = fid, /no_interactive_query, /no_realize
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  envi_file_query, fid, nb = nrlayers, ns = ns, nl = nl, dims = dims, data_type = type
  output = make_array(ns, nl, nrlayers, type = type)
  for b = 0, nrlayers - 1 do begin
    output[*, *, b] = envi_get_data(fid = fid, dims = dims, pos = b)
  endfor
  
  return, output
end

pro anomaly_handleGo, event
  ; Get the input paramaters
  inout = anomaly_get_inout(event)
  ; Get the active tab page
  fld = widget_info(event.top, find_by_uname = 'anomaly_scenarioTabs')
  curPage = widget_info(fld, /tab_current)
  case curPage of
    0: anomaly_handle_sequences, event, inout
    1: anomaly_handle_seasonal, event, inout
    2: anomaly_handle_change_detect, event, inout
  endcase
end

pro anomaly_handle_count_magnitude, event, inout, ndvi_py
  if n_tags(inout) eq 0 then return
  
  inputfile = inout.infile
  classfile = inout.classfile
  rangetablefile = inout.tblfile
  sdtablefile = inout.sdtblfile
  outfile   = inout.outfile
  onlyAggr  = inout.onlyAggr
  
  ; Get input needed
  fld = widget_info(event.top, find_by_uname = 'anomaly_outputCounting')
  widget_control, fld, get_value = cntname
  fld = widget_info(event.top, find_by_uname = 'anomaly_countingCount')
  widget_control, fld, get_value = cntcount_str
  fld = widget_info(event.top, find_by_uname = 'anomaly_magnitude')
  widget_control, fld, get_value = magname

  ; check input values
  doMag = 1
  if strlen(strtrim(cntname)) eq 0 then doMag = 0
  if strlen(strtrim(sdtablefile)) eq 0 then doMag = 0
  cntcount = fix(cntcount_str)
  if cntcount lt 0 then doMag = 0
  if strlen(strtrim(magname)) eq 0 then doMag = 0
  if doMag eq 0 then begin
    void = dialog_message('Missing or faulty input')
    return
  endif

  envi_open_file, inputfile, r_fid = ndvi, /no_realize, /no_interactive_query
  if ndvi eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  
  envi_file_query, ndvi, dims = dims, ns = ns, nl = nl, nb = nrlayers
  mi = envi_get_map_info(fid = ndvi, undefined = undefined)

  nrs_load_class_image, class, cldata = cldata, num_classes = nr_class
  lut = read_csv(rangetablefile, header = header)
  sdtable = read_csv(sdtablefile, header = sdheader)
  sd_tab = sdtable.(1)
  
  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Finding anomaly counts")
  progressBar -> Start

  nryears = nrlayers / ndvi_py
  cntoutput = bytarr(ns * nl, nryears)
  mag_out = fltarr(ns * nl, nryears)
  yearcnt = bytarr(ns * nl, ndvi_py)
  yearmag = fltarr(ns * nl, ndvi_py)
  outdata = bytarr(ns * nl)
  mag_data = fltarr(ns * nl)
  for y = 0, nryears - 1 do begin
    yearcnt[*] = 0
    yearmag[*] = 0.0
    ; calculate anomalies and magnitudes for an entire year
    p = y * ndvi_py
    for l = 0, ndvi_py - 1 do begin
      progressBar -> Update, 100.0 * (p + l + 1) / nrlayers, text = 'Progress: ' + string(p + l + 1, format = '(i0)') + ' of ' + string(nrlayers, format = '(i0)')
      
      layer = y * ndvi_py + l
      ldata = envi_get_data(fid = ndvi, dims = dims, pos = layer)
      anomaly_calc_from_sdrange, ldata, cldata, lut, nr_class, ndvi_py, layer, outdata = outdata
      anomaly_calc_magnitudes, ldata, cldata, sd_tab, nr_class, ndvi_py, layer, mag_data = mag_data
      yearcnt[*, l] = outdata
      yearmag[*, l] = mag_data
    endfor
    for p = 0L, ns * nl - 1L do begin
      ix = where(yearcnt[p, 0 : ndvi_py - 1] gt 0, count)
      cntoutput[p, y] = count ge cntcount
      if count ge cntcount then begin
        mag_ = yearmag[p, ix]
        mag_out[p, y] = total(mag_) / count
      endif
    endfor
  endfor
  bc = indgen(nryears) + 1
  bnames = string(bc, format = '("Year ",I02)')
  mnames = string(bc, format = '("Magnitude (year ",I02,")")')

  cntoutput = reform(cntoutput, ns, nl, nryears)
  outname = getoutname(cntname, postfix = '', ext = '.')  ; remove extension
  if undefined eq 1 then $
    envi_write_envi_file, cntoutput, out_name = outname, bnames = bnames  $
  else $
    envi_write_envi_file, cntoutput, out_name = outname, bnames = bnames, map_info = mi

  mag_out = reform(mag_out, ns, nl, nryears)
  outname = getoutname(magname, postfix = '', ext = '.')  ; remove extension
  if undefined eq 1 then $
    envi_write_envi_file, mag_out, out_name = outname, bnames = mnames  $
  else $
    envi_write_envi_file, mag_out, out_name = outname, bnames = mnames, map_info = mi
    
  progressBar -> Destroy
end

pro anomaly_handle_sequences, event, inout
  if n_tags(inout) eq 0 then return
  
  inputfile = inout.infile
  classfile = inout.classfile
  rangetablefile = inout.tblfile
  outfile   = inout.outfile
  onlyAggr  = inout.onlyAggr

  fld = widget_info(event.top, find_by_uname='anomaly_aggregate')
  doAggr = widget_info(fld, /button_set)
  fld = widget_info(event.top, find_by_uname='anomaly_counting')
  doCnt = widget_info(fld, /button_set)
  fld = widget_info(event.top, find_by_uname='anomaly_mag4count')
  doMag = widget_info(fld, /button_set)
  doMag *= doCnt
  
  if doAggr gt 0 then begin
    ; Consecutive option
    fld = widget_info(event.top, find_by_uname = 'anomaly_outputSequence')
    widget_control, fld, get_value = seqname
    if strlen(strtrim(seqname)) eq 0 then doAggr = 0
    
    fld = widget_info(event.top, find_by_uname = 'anomaly_sequenceCount')
    widget_control, fld, get_value = seqcount_str
    
    seqcount = fix(seqcount_str)
    if seqcount lt 0 then doAggr = 0
  endif

  if doCnt gt 0 then begin
    ; Counting anomalies option
    fld = widget_info(event.top, find_by_uname = 'anomaly_outputCounting')
    widget_control, fld, get_value = cntname
    if strlen(strtrim(cntname)) eq 0 then doCnt = 0
    
    fld = widget_info(event.top, find_by_uname = 'anomaly_countingCount')
    widget_control, fld, get_value = cntcount_str
    
    cntcount = fix(cntcount_str)
    if cntcount lt 0 then doCnt = 0
  endif
  
  ndvi_py = 36  ; number of layers per year (10 day products)
  
  if doMag eq 1 then begin
    if onlyAggr then begin
      ans = dialog_message('Need NDVI input file; skip probability of change output?', /question)
      if ans ne 'Yes' then return
      
      doMag = 0 
    endif
  endif
    
  ; Calculate anomalies
  if onlyAggr eq 0 then begin
    cancelled = anomaly_calc_anomalies(inputfile, classfile, rangetablefile, ndvi_py, outfile, output, undefined)
    if cancelled eq 1 then return
  endif 

  if doMag eq 1 then begin
    ; also takes care of the count option
    anomaly_handle_count_magnitude, event, inout, ndvi_py
    doCnt = 0
  endif
  
  if (doAggr eq 1) or (doCnt eq 1) then begin
  ; initialise tranquilizer
    progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Finding sequences")
    progressBar -> Start
  
    if onlyAggr or (n_elements(output) le 1) then begin
      output = anomaly_load_output(outfile, undefined)
    endif
    
    sz = size(output, /dim)
    ns = sz[0]
    nl = sz[1]
    nrlayers = sz[2]
    nryears = nrlayers / ndvi_py
    output = reform(output, ns * nl, nrlayers, /overwrite)
    if doAggr eq 1 then seqoutput = bytarr(ns * nl, nryears)
    if doCnt  eq 1 then cntoutput = bytarr(ns * nl, nryears)
    for p = 0L, ns * nl - 1L do begin
      if (p mod ns) eq 0 then $
        progressBar -> Update, 100.0 * p / (ns * nl - 1), text = 'Progress: ' + string(p, format = '(i0)') + ' of ' + string(ns * nl, format = '(i0)')
      for y = 0, nryears - 1 do begin
        b = y * ndvi_py
        e = b + ndvi_py - 1
        if doaggr eq 1 then begin
          nrs_calc_seq, output[p, b:e], mx
          seqoutput[p, y] = mx ge seqcount
        endif
        if doCnt eq 1 then begin
          dut = output[p, b:e]
          ix = where(dut gt 0, count)
          cntoutput[p, y] = count ge cntcount
        endif
      endfor
    endfor
    bc = indgen(nryears) + 1
    bnames = string(bc, format = '("Year ",I02)')

    if doAggr eq 1 then begin
      seqoutput = reform(seqoutput, ns, nl, nryears)
      outname = getoutname(seqname, postfix = '', ext = '.')  ; remove extension
      
      if undefined eq 1 then $
        envi_write_envi_file, seqoutput, out_name = outname, bnames = bnames  $
      else $
        envi_write_envi_file, seqoutput, out_name = outname, bnames = bnames, map_info = mi
    endif
    
    if doCnt  eq 1 then begin
      cntoutput = reform(cntoutput, ns, nl, nryears)
      outname = getoutname(cntname, postfix = '', ext = '.')  ; remove extension

      if undefined eq 1 then $
        envi_write_envi_file, cntoutput, out_name = outname, bnames = bnames  $
      else $
        envi_write_envi_file, cntoutput, out_name = outname, bnames = bnames, map_info = mi
    endif
    
    progressBar -> Destroy
  endif
end

pro anomaly_handle_seasonal, event, inout
  if n_tags(inout) eq 0 then return
  
  inputfile = inout.infile
  classfile = inout.classfile
  tablefile = inout.tblfile
  outfile   = inout.outfile
  onlyAggr  = inout.onlyAggr

  fld = widget_info(event.top, find_by_uname = 'anomaly_outputSeasons')
  widget_control, fld, get_value = outseasons
  if strlen(strtrim(outseasons)) eq 0 then return

  ndvi_py = 36  ; number of layers per year (10 day products)
  ; Calculate anomalies
  if onlyAggr eq 0 then begin
    cancelled = anomaly_calc_anomalies(inputfile, classfile, tablefile, ndvi_py, outfile, output, undefined)
    if cancelled eq 1 then return
  endif
  
  ; if only on disk, get output of anomaly calculation
  if onlyAggr or (n_elements(output) le 1) then begin
    output = anomaly_load_output(outfile, undefined)
  endif
  
  sz = size(output, /dim)
  ns = sz[0]
  nl = sz[1]
  nrlayers = sz[2]
  nryears = nrlayers / ndvi_py
  nrseasons = nryears * 2
  output = reform(output, ns * nl, nrlayers, /overwrite)
  
  ; add handler for number conversion errors 
  catch, error_status
  
  if error_status ne 0 then begin
    catch, /cancel
    ans = dialog_message('Layer numbers not all filled in, or no number', /error)
    return
  endif
  
  ; Get values out of all UI elements
  season = {first:0, last:0, perc:0.0}
  seasons = replicate(season, nrseasons)
  for i = 1, nrseasons do begin
    numstr = string(i - 1, format = '(i03)')
    uname = 'seasfirst_' + numstr
    fld = widget_info(event.top, find_by_uname = uname)
    widget_control, fld, get_value = val
    seasons[i - 1].first = fix(val) - 1
    
    uname = 'seaslast_' + numstr
    fld = widget_info(event.top, find_by_uname = uname)
    widget_control, fld, get_value = val
    seasons[i - 1].last = fix(val) - 1
    
    uname = 'prop_' + numstr
    fld = widget_info(event.top, find_by_uname = uname)
    widget_control, fld, get_value = val
    seasons[i - 1].perc = float(val) / 100.0
  endfor
  catch, /cancel  ; disable number conversion error handler
  
  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Finding seasonal anomalies")
  progressBar -> Start
  
  seas_out = bytarr(ns * nl, nrseasons)
  for p = 0L, ns * nl - 1L do begin
    if (p mod ns) eq 0 then $
      progressBar -> Update, 100.0 * p / (ns * nl - 1), text = 'Progress: ' + string(p, format = '(i0)') + ' of ' + string(ns * nl, format = '(i0)')

    ; now for all seasons
    for i = 0, nrseasons - 1 do begin
      b1 = seasons[i].first + ndvi_py * (i / 2)
      b2 = seasons[i].last + ndvi_py * (i / 2)
      perc = seasons[i].perc
      ix = where(output[p,b1:b2] ne 0, count)
      seas_out[p, i] = (1.0 * count / (b2 - b1 + 1)) gt perc
    endfor
  endfor
  
  seas_out = reform(seas_out, ns, nl, nrseasons, /overwrite)
  
  ; store the result
  outname = getoutname(outseasons, postfix = '', ext = '.')  ; remove extension

  ; build band names
  nn = indgen(nrseasons)
  bnames = 'Season ' + string(1 + nn / 2, format = '(I0)') + '.' + string(1 + nn mod 2, format = '(I0)')

  if undefined eq 1 then $
    envi_write_envi_file, seas_out, out_name = outname, bnames = bnames  $
  else $
    envi_write_envi_file, seas_out, out_name = outname, bnames = bnames, map_info = mi
    
  progressBar -> Destroy
end

pro anomaly_handle_change_detect, event, inout
  if n_tags(inout) eq 0 then return
  
  inputfile = inout.infile
  classfile = inout.classfile
  tablefile = inout.sdtblfile
  outfile   = inout.outfile
  onlyAggr  = inout.onlyAggr
  if onlyAggr eq 1 then return

  ; get second class image if available
  fld = widget_info(event.top, find_by_uname = 'anomaly_refimg2')
  widget_control, fld, get_value = classname2

  fld = widget_info(event.top, find_by_uname = 'anomaly_mask')
  widget_control, fld, get_value = maskname

  fld = widget_info(event.top, find_by_uname = 'anomaly_mag_output')
  widget_control, fld, get_value = magname
  
  magnitude = strtrim(magname, 2)

  ndvi_py = 36  ; number of layers per year (10 day products)
  
  envi_open_file, inputfile, r_fid = ndvi
  if ndvi eq -1 then return
  
  envi_open_file, classfile, r_fid = class
  if class eq -1 then return
  
  envi_file_query, class, dims = dims
  mi_ref = envi_get_map_info(fid = class, undefined = undef_csy)

  cln2 = strtrim(classname2)
  class2 = -1
  if strlen(cln2) gt 0 then $
    envi_open_file, cln2, r_fid = class2
    
  mask = strtrim(maskname)
  m_fid = -1
  if strlen(mask) gt 0 then $
    envi_open_file, mask, r_fid = m_fid

  if class2 gt 0 then begin
    nrs_ndvi_change_detection, class, ndvi, tablefile, outdata, bnames = bnames, ndvi_py = ndvi_py, ref_img2 = class2, mag_data = mag_data
    if n_elements(outdata) eq 1 then return ; user stopped calculation
    
    ; now apply masking if specified
    if m_fid gt 0 then begin
      sz = size(outdata, /dim)
      md = envi_get_data(fid = m_fid, dims = dims, pos = 0)
      outdata = reform(outdata, sz[0] * sz[1], sz[2], /overwrite)
      mag_data = reform(mag_data, sz[0] * sz[1], sz[2], /overwrite)
      ix = where(md eq 0, count)
      if count gt 0 then begin
        outdata[ix, *] = 0
        mag_data[ix, *] = 0
      endif
      outdata = reform(outdata, sz[0], sz[1], sz[2], /overwrite)
      mag_data = reform(mag_data, sz[0], sz[1], sz[2], /overwrite)
    endif
      
  endif else begin
    nrs_ndvi_change_detection, class, ndvi, tablefile, outdata, bnames = bnames, ndvi_py = ndvi_py, mag_data = mag_data
    if n_elements(outdata) eq 1 then return ; user stopped calculation
  endelse
  
  outname = getoutname(outfile, postfix = '', ext = '.')  ; remove extension
  outmagnitude = getoutname(outfile, postfix = '_mag', ext = '.')  ; remove extension
  
  if undef_csy eq 1 then begin
    envi_write_envi_file, outdata, out_name = outname, bnames = bnames
    if strlen(magnitude) gt 0 then $
      envi_write_envi_file, mag_data, out_name = magnitude, bnames = bnames
  endif else begin
    envi_write_envi_file, outdata, out_name = outname, bnames = bnames, map_info = mi_ref
    if strlen(magnitude) gt 0 then $
      envi_write_envi_file, mag_data, out_name = magnitude, bnames = bnames, map_info = mi_ref
  endelse

end
