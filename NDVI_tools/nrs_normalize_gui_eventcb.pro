pro nrs_normalize_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_input_image')
  widget_control, val_fld, get_value = input
  
  input = strtrim(input, 2)
  if strlen(input) eq 0 then return

  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_outputFile')
  widget_control, val_fld, get_value = outname
  if strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(input, ext = '.', postfix = '_norm')
    widget_control, val_fld, set_value = outname
  endif
end

pro nrs_normalize_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_use_perc_button')
  usePercentage = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_input_image')
  widget_control, val_fld, get_value = input
  input = strtrim(input, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_period')
  widget_control, val_fld, get_value = norm_option
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_minmax')
  widget_control, val_fld, get_value = minmax_option
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_normalize_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  if strlen(input) eq 0 then begin
    void = error_message('Input series not specified!', traceback = 0, /error)
    return
  endif
  
  if strlen(outname) eq 0 then begin
    void = error_message('Output name not specified!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Normalize series" $
                        , /fast_loop $
                        )
  
  nrs_normalize_index, input $
                       , outname = outname $
                       , multi_year = norm_option eq 0 $
                       , percentage = usePercentage $
                       , from_max = minmax_option $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar -> Destroy
end

