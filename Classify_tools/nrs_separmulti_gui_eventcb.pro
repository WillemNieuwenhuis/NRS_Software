pro nrs_separmulti_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_separ', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_separmulti_handleOK, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_classmap')
  widget_control, val_fld, get_value = class_list
  class_list = strtrim(class_list, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_mat_button')
  doMatrix = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_separmulti_append_button')
  doAppend = widget_info(val_fld, /button_set)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input image stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input image stack could not be opened!', traceback = 0, /error)
    return
  endif
  
  if strlen(class_list) eq 0 then begin
    void = error_message('List file not specified!', traceback = 0, /error)
    return
  endif

  if ~(file_info(class_list)).exists then begin
    void = error_message('List file not found', traceback = 0, /error)
    return
  endif

  lbl = strarr(file_lines(class_list))
  openr, lun, class_list, /get_lun
  readf, lun, lbl
  close, lun
  free_lun, lun
  lst = strtrim(lbl, 2)
  
  ; outer tranquilizer
  prog_obj_outer = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Automate separability analysis" $
                        , /fast_loop $
                        )

  ; inner tranquilizer
  prog_obj_inner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Separability analysis" $
                        , /fast_loop $
                        )

  nrs_set_progress_property, prog_obj_outer, title = 'Checking images', /start, xs = 600, ys = 100
  nrs_set_progress_property, prog_obj_inner, xs = 650, ys = 150

  cl_maps = []
  for fn = 0, n_elements(lst) - 1 do begin
    if nrs_update_progress(prog_obj_outer, fn, n_elements(lst), cancelled = cancelled) then return
    
    envi_open_file, lst[fn], r_fid = fid, /no_realize, /no_interactive_query
    if fid eq -1 then continue
    
    envi_file_query, fid, file_type = ft_cl
    if ft_cl ne 3 then continue    ; ENVI classification == 3

    cl_maps = [cl_maps, lst[fn]]
    envi_file_mng, id = fid, /remove
  endfor

  nrs_set_progress_property, prog_obj_outer, title = 'Analysing class images', /start
  
  matname = getOutname(basename, postfix = '_mat', ext = '.csv')
  nrs_log_line, basename, 'nr_classes,min_sep,avg_sep', append = doAppend ; write header
  nrs_log_line, matname, 'Covariance matrices', append = doAppend
  
  nf = n_elements(cl_maps)
  for f = 0, nf - 1 do begin
    class_img = cl_maps[f]
    if nrs_update_progress(prog_obj_outer, f, nf, cancelled = cancelled) then return

    nrs_class_separability, ref, class_img, sep_avg = sep_avg, sep_min = sep_min $
                          , separ_matrix = separ $
                          , prog_obj = prog_obj_inner, cancelled = cancelled
    
    if ~cancelled then begin
      envi_open_file, lst[f], r_fid = fid, /no_realize, /no_interactive_query
      envi_file_query, fid, num_classes = nr_class, class_names = cnames
      ix = where(strlowcase(cnames) eq 'unclassified', cnt)
      if cnt gt 0 then nr_class -= 1
      envi_file_mng, id = fid, /remove
      nrs_log_line, basename, string([nr_class, sep_min, sep_avg], format = '(i3,",",i0,",",i0)'), /append
      
      if doMatrix then begin
        nrs_write_separ_mat, separ, outname = matname, cnames = cnames, /append
      endif
    endif

    if cancelled then break
  endfor

  if prog_obj_inner ne !null then $
    prog_obj_inner -> Destroy
  if prog_obj_outer ne !null then $
    prog_obj_outer -> Destroy
end

