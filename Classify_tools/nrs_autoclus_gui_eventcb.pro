pro nrs_autoclus_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target[0])
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_iso', ext = '.dat')
  outtable = getOutname(target_str, postfix = '_separ', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_outputFile')
  widget_control, val_fld, set_value = basename
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_outputTable')
  widget_control, val_fld, set_value = outtable
end

pro nrs_autoclus_handleOK, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_outputFile')
  widget_control, val_fld, get_value = basename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_outputTable')
  widget_control, val_fld, get_value = separ_tbl
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_minclasses')
  widget_control, val_fld, get_value = str_minclass

  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_maxclasses')
  widget_control, val_fld, get_value = str_maxclass
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_iterations')
  widget_control, val_fld, get_value = str_iter
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_mask_button')
  doNoMask = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_stat_button')
  doSaveMat = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_autoclus_append_button')
  doAppend = widget_info(val_fld, /button_set)
  
  min_class = fix(str_minclass)
  max_class = fix(str_maxclass)
  iterations = fix(str_iter)
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input image stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  
  if fid_ref eq -1 then begin
    void = error_message('Input image stack could not be opened!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Auto cluster" $
                        , /fast_loop $
                        )
  
  if ~doNoMask then begin
    envi_file_query, fid_ref, dims = dims, nb = nb, ns = ns, nl = nl
    data = envi_get_data(fid = fid_ref, dims = dims, pos = [0])
    mi = envi_get_map_info(fid = fid_ref, undef = undef)
    if undef then delvar, mi
    mask = data ne 0
    mask_name = getOutname(ref, postfix = '_mask', ext = '.')
    envi_write_envi_file, mask, out_name = mask_name, bnames = ['Mask Band'], map_info = mi, /no_open
  endif
  
  if doSaveMat then stat_out = separ_tbl
  nrs_auto_cluster, ref, run_range = [min_class, max_class], basename = basename $
                    , mask_name = mask_name $
                    , stat_out = stat_out $
                    , append = doAppend $
                    , outtable = separ_tbl $
                    , iterations = iterations $
                    , prog_obj = progressBar, cancelled = cancelled
  
  if progressBar ne !null then $
    progressBar -> Destroy
end

