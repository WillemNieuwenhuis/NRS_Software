pro nrs_separability_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_separ', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_outputFile')
  widget_control, val_fld, set_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_InfoText')
  widget_control, val_fld, set_value = 'Separability result: ---'
end

pro nrs_separability_handleOK, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_classmap')
  widget_control, val_fld, get_value = class_img
  class_img = strtrim(class_img, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_mat_button')
  doMatrix = widget_info(val_fld, /button_set)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input image stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input image stack could not be opened!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, class_img, r_fid = fid_cl, /no_realize, /no_interactive_query
  
  if fid_cl eq -1 then begin
    void = error_message('Input classified image could not be opened!', traceback = 0, /error)
    return
  endif
  
  envi_file_query, fid_cl, num_classes = nr_class, class_names = cnames
  ix = where(strlowcase(cnames) eq 'unclassified', cnt)
  if cnt gt 0 then nr_class -= 1
  
    ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Separability analysis" $
                        , /fast_loop $
                        )

  nrs_class_separability, ref, class_img, sep_avg = sep_avg, sep_min = sep_min $
                        , separ_matrix = separ $
                        , prog_obj = prog_obj, cancelled = cancelled
  
  if ~cancelled then begin
    nrs_log_line, basename, string([nr_class, sep_min, sep_avg], format = '(i3,",",i0,",",i0)'), /append
    
    crlf = string(13b) + string(10b)
    msg = 'Separability result:' + crlf $
        + '    minimum = ' + string(sep_min, format = '(i0)') + crlf $
        + '    average = ' + string(sep_avg, format = '(i0)') + crlf

    val_fld = widget_info(event.top, find_by_uname = 'nrs_separability_InfoText')
    widget_control, val_fld, set_value = msg
    
    if doMatrix then begin
      outname = getOutname(basename, postfix = '_mat', ext = '.csv')
      nrs_write_separ_mat, separ, outname = outname, cnames = cnames, /append
    endif
  endif
  
  if prog_obj ne !null then $
    prog_obj -> Destroy
end

