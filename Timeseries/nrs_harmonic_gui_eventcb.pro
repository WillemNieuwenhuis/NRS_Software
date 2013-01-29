pro nrs_harmonic_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_har', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_harmonic_max_change, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_max_harm')
  widget_control, val_fld, get_value = str_maxh
  max_harm = fix(str_maxh)
  
  if max_harm le 0 then begin
    formula = ''
  endif else begin
    crlf = string(13b) + string(10b)
    ix = fix(indgen(3 * max_harm) / 3) + 1
    f_part = string(ix, format = '("A", i0, " * cos(", i0, "ft - Ph", i0, ")")')
    formula = 'Composition formula: ' + crlf $
              + '    Mean + ' + strjoin(f_part, ' + ') + crlf $
              + ' where: Ai = amplitude, Phi = Phase, f = frequency (days), t = time'
  endelse
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_InfoText')
  widget_control, val_fld, set_value = formula
end

pro nrs_harmonic_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_img_py')
  widget_control, val_fld, get_value = str_py

  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_max_harm')
  widget_control, val_fld, get_value = str_maxh
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_degrees_button')
  doDegrees = widget_info(val_fld, /button_set)
  
  img_py = fix(str_py)
  max_harm = fix(str_maxh)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Harmonic analysis" $
                        , /fast_loop $
                        )
  
  nrs_harmonic_analysis, ref, max_harm, outname = basename, img_py = img_py, degrees = doDegrees, prog_obj = progressBar, cancelled = cancelled
  
  if progressBar ne !null then $
    progressBar -> Destroy
end

