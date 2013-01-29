pro nrs_stack_stat_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_inputImage')
  widget_control, fld, get_value = inputfile

  if strlen(strtrim(inputfile)) eq 0 then return

  outputtable = getOutname(inputfile, basename = 'poolsd', postfix = '', ext = '.csv')
  
  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_outputTable')
  widget_control, fld, get_value = ot
  if strlen(strtrim(ot)) eq 0 then $
    widget_control, fld, set_value = outputtable
end

pro nrs_stack_stat_handleGo, Event
  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_inputImage')
  widget_control, fld, get_value = inputfile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_img_py')
  widget_control, fld, get_value = ndvilayers

  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_classes')
  widget_control, fld, get_value = classfile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_stack_stat_outputTable')
  widget_control, fld, get_value = outputtable

  ; now check all inputs
  if strlen(strtrim(inputfile)) eq 0 then begin
    void = error_message('Missing input stack', title = 'Input error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(classfile)) eq 0 then begin
    void = error_message('Missing class image', title = 'Input error', /error, /noname, traceback = 0)
    return
  endif
  if strlen(strtrim(outputtable)) eq 0 then begin
    void = error_message('Missing output name', title = 'Input error', /error, /noname, traceback = 0)
    return
  endif
  
  img_per_year = fix(strtrim(ndvilayers[0]))
  if (img_per_year lt 10) or (img_per_year gt 36) then begin
    void = error_message('Number of layers expected between 10 and 36', title = 'Input error', /error, /noname, traceback = 0)
    return
  endif
  
  ; open inputs
  envi_open_file, inputfile, r_fid = stack, /no_realize, /no_interactive_query
  if stack eq -1 then return
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then return
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass
  
  envi_file_query, stack, dims = dims, ns = ns, nl = nl, nb = nrlayers 
  mi_ref = envi_get_map_info(fid = stack, undefined = undef_csy)

  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Calculate pooled standard deviation")
  progressBar -> Start

  nrs_calc_class_stddev, stack, class, stdevs = stats, prog_obj = progressBar
  
  nrs_calc_pooled_sd, stats, img_per_year, poolsd = poolsd
  
  cl_col = strarr(n_elements(poolsd))
  for cl = 0, nrclass - 1 do begin
    cl_col[cl * img_per_year : (cl + 1) * img_per_year - 1] = string(cl + 1, format = '("Class ", i0)') 
  endfor
  
  write_csv, outputtable, cl_col, poolsd, header = ['Class','Pooled_SD']
  
  progressBar -> Destroy
end
