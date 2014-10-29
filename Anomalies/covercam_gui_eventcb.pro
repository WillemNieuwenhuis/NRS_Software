pro covercam_handleBrowseInput, event
  compile_opt idl2, logical_predicate
  
  fld = widget_info(event.top, find_by_uname = 'covercam_inputImage')
  widget_control, fld, get_value = inputfile
  len = strlen(strtrim(inputfile))
  
  fld = widget_info(event.top, find_by_uname = 'covercam_year_panel')
  widget_control, fld, sensitiv = len gt 0
  fld = widget_info(event.top, find_by_uname = 'covercam_time_panel')
  widget_control, fld, sensitiv = len gt 0
  
  if strlen(strtrim(inputfile)) eq 0 then return
  
  magfile = getOutname(inputfile, postfix = '_mag', ext = '.')

  covercam_handle_NPY_change, event
  
  fld = widget_info(event.top, find_by_uname = 'covercam_magnitude')
  widget_control, fld, get_value = ofile
  if strlen(strtrim(ofile)) eq 0 then $
    widget_control, fld, set_value = magfile
end

pro covercam_handle_NPY_change, event
  compile_opt idl2, logical_predicate
  
  fld = widget_info(event.top, find_by_uname = 'covercam_inputImage')
  widget_control, fld, get_value = inputfile
  
  nb = -1
  if strlen(strtrim(inputfile)) gt 0 then begin
    envi_open_file, inputfile, r_fid = fid, /no_realize, /no_interactive_query
    if fid ne -1 then $
      envi_file_query, fid, nb = nb
  endif
  
  fld = widget_info(event.top, find_by_uname = 'covercam_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(ndvilayers)
  
  fld = widget_info(event.top, find_by_uname = 'covercam_time_to')
  widget_control, fld, get_uvalue = to  ; the edit field object
  toObj = (to.object)->getid()
  widget_control, toObj, get_uvalue = to  ; set to non-zero if manual user change
  widget_control, fld, get_value = user_to  ; the visible value
  ; if no manual value change than follow the change in number of layers
  to = fix(to)
  if (to eq 0) or (to gt ndvi_py) then begin
    widget_control, fld, set_value = string(ndvi_py, format = '(i2)')
  endif else begin
    widget_control, fld, set_value = string(to, format = '(i2)')
  endelse
  
  covercam_update_combo, event, nb, ndvi_py
end

function covercam_handle_time_ft, event
  fld = widget_info(event.top, find_by_uname = 'covercam_ndvipy')
  widget_control, fld, get_value = ndvilayers
  ndvi_py = fix(ndvilayers)
  
  fld = widget_info(event.top, find_by_uname = 'covercam_time_to')
  widget_control, fld, get_uvalue = to  ; set to non-zero if manual user change
  widget_control, fld, get_value = user_to
  toObj = (to.Object)->getID()
  
  if user_to ne ndvi_py then begin
    widget_control, toObj, set_uvalue = fix(user_to)  ; set user changed value
  endif
end

pro covercam_update_combo, event, nb, ndvi_py
  compile_opt idl2, logical_predicate
  
  if nb gt 0 then begin
    nryears = nb / ndvi_py
    ystr = string(indgen(nryears) + 1, format = '(i0)')
    
    fld = widget_info(event.top, find_by_uname = 'covercam_years_combo')
    widget_control, fld, set_value = [ 'All', ystr]
    widget_control, fld, set_combobox_select = 0
  endif
  
end

pro covercam_handleGo, event
  compile_opt idl2, logical_predicate

  ; collect parameters
  fld = widget_info(event.top, find_by_uname = 'covercam_inputImage')
  widget_control, fld, get_value = inputfile

  fld = widget_info(event.top, find_by_uname = 'covercam_ndvipy')
  widget_control, fld, get_value = ndvilayers

  fld = widget_info(event.top, find_by_uname = 'covercam_years_combo')
  sel_year = widget_info(fld, /combobox_gettext)

  fld = widget_info(event.top, find_by_uname = 'covercam_time_from')
  widget_control, fld, get_value = fromtime
    
  fld = widget_info(event.top, find_by_uname = 'covercam_time_to')
  widget_control, fld, get_value = totime

  fld = widget_info(event.top, find_by_uname = 'covercam_classes')
  widget_control, fld, get_value = classfile

  fld = widget_info(event.top, find_by_uname = 'covercam_refimage')
  widget_control, fld, get_value = refimage

  fld = widget_info(event.top, find_by_uname = 'covercam_magnitude')
  widget_control, fld, get_value = magname
  
  fld = widget_info(event.top, find_by_uname = 'change_detection_sd_muly_combo')
  sd_mult_str = widget_info(fld, /combobox_gettext)
  
  fld = widget_info(event.top, find_by_uname = 'covercam_mask')
  widget_control, fld, get_value = pixmask_str
  
  fld = widget_info(event.top, find_by_uname = 'covercam_absdiff')
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
  sd_mult = float(strtrim(sd_mult_str[0]))

  pixmask = fix(strtrim(pixmask_str[0]))
  if pixmask lt 0 then begin
    void = error_message('Number of pixels cannot be negative', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif

  if sel_year eq 'All' then sel_year = -1 else sel_year = fix(sel_year[0])
  totime = min([fix(totime[0]), ndvi_py])
  fromtime = max([1, fix(fromtime[0])])

  if (strlen(strtrim(magname)) eq 0) then begin
    void = error_message('Probability of change output not specified', title = 'Change detection error', /error, /noname, traceback = 0)
    return
  endif
  
  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Detect NDVI changes" $
                        , /fast_loop $
                        )

  covercam_calc, inputfile, refimage, classfile, ndvi_py, sd_mult, pixmask $
                      , fromtime, totime, sel_year $
                      , abs_diff $
                      , magname $
                      , prog_obj = progressBar, cancelled = cancelled
    
  if obj_valid(progressBar) then progressBar -> Destroy
  
end
