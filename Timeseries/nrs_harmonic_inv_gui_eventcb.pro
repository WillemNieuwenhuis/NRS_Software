pro nrs_harmonic_inv_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_hcom', ext = '.')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_harmonic_inv_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_years')
  widget_control, val_fld, get_value = str_nr_years
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_max_harm')
  widget_control, val_fld, get_value = str_harm
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_harmonic_inv_img_py')
  widget_control, val_fld, get_value = str_img_py
  
  nr_years = fix(str_nr_years[0])  ; number of years in input data
  nr_harm = fix(str_harm[0])   ; number of harmonics in input
  img_py = fix(str_img_py[0])  ; number of images per year in output
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input harmonic decomposition not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input harmonic decomposition could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Harmonic composition" $
                        , /fast_loop $
                        )
  
  nrs_harmonic_composition, ref, outname = basename $
                          , nr_years, harmonics = nr_harm $
                          , img_py = img_py, prog_obj = progressBar, cancelled = cancelled
  
  if progressBar ne !null then $
    progressBar -> Destroy
end

