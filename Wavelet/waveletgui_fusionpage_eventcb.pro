pro wav_browseLabelImage, event, fieldname, labelname
  fld = widget_info(event.top, find_by_uname=fieldname)
  if fld eq -1 then return
  
  lbl = widget_info(event.top, find_by_uname=labelname)
  if lbl eq -1 then return
  
  widget_control, fld, get_value = filename
  if strlen(strtrim(filename)) le 0 then return
  
  envi_open_file, filename, /no_interactive_query, /no_realize, r_fid=fid
  
  if fid eq -1 then begin
    ans = dialog_message('Could not open or find "' + filename + '"', title = 'Wavelet', /error)
    return
  endif
  
  envi_file_query, fid, nb = nb, nl = nl, ns = ns
  
  str = 'Dimensions: ' + string(ns, format='(i0)') + ' x ' + string(nl, format='(i0)')
  widget_control, lbl, set_value = str 
end

pro wav_handleBrowseHiresImage, event
  wav_browseLabelImage, event, 'hiresImageField', 'hiresImageInfoLabel'
end

pro wav_handleBrowseLoresImage, event
  wav_browseLabelImage, event, 'loresImageField', 'loresImageInfoLabel'
end

pro wav_handleFusionCalculate, event
  widget_control, event.top, get_uvalue=state
  id = Widget_Info(event.top, FIND_BY_UNAME='hiresImageField')
  widget_control, id, get_value = hiresFilename
  id = Widget_Info(event.top, FIND_BY_UNAME='loresImageField')
  widget_control, id, get_value = loresFilename
  id = widget_info(event.top, find_by_uname='OrderFusionText')
  widget_control, id, get_value = orderString
  id = widget_info(event.top, find_by_uname='FamilyFusionComboBox')
  family = widget_info(id, /combobox_gettext)
  id = widget_info(event.top, find_by_uname='FusionOutputText')
  widget_control, id, get_value = outputFusion
  id = widget_info(event.top, find_by_uname='fusionInjectType')
  widget_control, id, get_value = injection
  id = widget_info(event.top, find_by_uname='fusionNormalizeToggle')
  useNormalize = widget_info(id, /button_set)
    
  order = 0

  on_ioerror, bad_order
  order = fix(orderString)
  names = strarr(1)

  ; reset error_handler
  on_ioerror, NULL
  
  envi_open_file, hiresFilename, r_fid = fidHi, /no_interactive_query, /no_realize
  if fidHi eq -1 then begin
    ans = Dialog_Message('Could not open high resolution input image', /error, title='Wavelet error')
    return
  endif
  
  envi_open_file, loresFilename, r_fid = fidLo, /no_interactive_query, /no_realize
  if fidLo eq -1 then begin
    ans = Dialog_Message('Could not open low resolution input image', /error, title='Wavelet error')
    return
  endif

  if (strlen(outputFusion) eq 0) then begin
    ans = Dialog_Message('No output name specified', /error, title='Wavelet error')
    return
  endif

  miHi = envi_get_map_info(fid = fidHi)
  miLo = envi_get_map_info(fid = fidLo)
  envi_file_query, fidHi, dims = dimsHi, ns = nsHi, nl = nlHi, data_type = dtHi
  envi_file_query, fidLo, nb = nbLo, dims = dimsLo, data_type = dtLo
    
  ; calculate the number of decompositions of the high res image
  ; to reach the level of the lores image
  levelsHi = 1 + round(alog(miLo.ps[0] / miHi.ps[0]) / alog(2))
  
  img_hi = envi_get_data(fid=fidHi, dims = dimsHi, pos = 0)

  if useNormalize eq 1 then begin 
    ; calculate the coefficients required for normalization
    wavelet_calc_mem, img_hi, coefHi_norm, family, order, levelsHi + 1, namesNorm, crystalsNorm
    s1d1Norm = wav_get_crystal(img_hi, levelsHi + 1, 0)
    d1s1Norm = wav_get_crystal(img_hi, levelsHi + 1, 1)
    d1d1Norm = wav_get_crystal(img_hi, levelsHi + 1, 2)
  endif
  ; calculate the coefficients for injection
  wavelet_calc_mem, img_hi, coefHi, family, order, levelsHi, namesHi, crystalsHi
  num = n_elements(crystalsHi) / 4
  bounds = crystalsHi[*, num - 1]
  left  = bounds[0]
  top   = bounds[1]
  right = bounds[2]
  bottom = bounds[3]
  
  fusedImage = make_array(nsHi, nlHi, nbLo, type = dtHi)
  ; calculate fusion for all lores bands
  for b = 0, nbLo - 1 do begin
    img_lo = envi_get_data(fid = fidLo, dims = dimsLo, pos = b)
    
    inject = coefHi ; prepare a copy of the hires coefficients for injection
    if useNormalize eq 1 then begin 
      wavelet_calc_mem, img_lo, coefInj, family, order, 1, nameInj, crystalsInj
      s1d1Inj = wav_get_crystal(coefInj, 1, 0)
      d1s1Inj = wav_get_crystal(coefInj, 1, 1)
      d1d1Inj = wav_get_crystal(coefInj, 1, 2)
      
      a = fltarr(3)
      b = fltarr(3)
      a[0] = stddev(s1d1Inj) / stddev(s1d1Norm)
      b[0] = mean(s1d1Inj) - a[0] * mean(s1d1Norm)
      a[1] = stddev(d1s1Inj) / stddev(d1s1Norm)
      b[1] = mean(d1s1Inj) - a[1] * mean(d1s1Norm)
      a[2] = stddev(d1d1Inj) / stddev(d1d1Norm)
      b[2] = mean(d1d1Inj) - a[2] * mean(d1d1Norm)
      
      for lv = 0, levelsHi do begin
        wav_normalize_crystal, inject, lv, lv * 3 + 0, a[0], b[0]
        wav_normalize_crystal, inject, lv, lv * 3 + 1, a[1], b[1]
        wav_normalize_crystal, inject, lv, lv * 3 + 2, a[2], b[2]
      endfor
      
    endif
    
    if injection eq 0 then begin  ; replace
      inject[left:right, top:bottom] = img_lo
    endif else begin  ; average
      inject[left:right, top:bottom] += img_lo
      inject[left:right, top:bottom] /= 2
    endelse
    
    fusedImage[*, *, b] = inject
    
  endfor

  wavelet_calc_inv_mem, fusedImage, inv, family, order, levels

  envi_write_envi_file, inv, out_name = outputFusion, map_info = miHi
  envi_write_envi_file, fusedImage, out_name = outputFusion+"_raw", map_info = miHi
  
  goto, done

bad_order:
  if (order le 0) then begin
    ans = Dialog_Message('Order must be a none-zero integer number', /error, title='Wavelet error')
    return
  endif

bad_level:
  if (levels le 0) then begin
    ans = Dialog_Message('Level must be a none-zero integer number', /error, title='Wavelet error')
    return
  endif
  
done:
  ans = Dialog_Message('Wavelet calculation finished', /information, title='Wavelet Information')

end
