pro svc_handleInputFileChange, event
  val_fld = widget_info(event.top, find_by_uname = 'svc_input_file')
  widget_control, val_fld, get_value = src_filename

  infile = strtrim(src_filename)
  if strlen(infile) eq 0 then return
  
  is_csv = query_csv(infile, info)
  
  if is_csv eq 0 then begin
    pos = strpos(infile, path_sep(), /reverse_search)
    if pos gt 0 then fname = strmid(infile, pos + 1)
    ans = message_dialog(fname + ' is not a CSV file; select another')
    return
  endif
  
  ; fill list with field names from the CSV file
  csv_table = read_csv(infile, num_records = 1, header = header, count = rec_count)
  val_fld = widget_info(event.top, find_by_uname = 'svc_field_list')
  widget_control, val_fld, set_value = header
  
  ext = ''
  base = file_basename(infile)
  pos = strpos(base, '.', /reverse_search)
  if pos gt 0 then begin
    file = strmid(base, 0, pos)
    ext = strmid(base, pos)
  endif
  outfile = file_dirname(infile) + path_sep() + file + '_out' + ext
  
  val_fld = widget_info(event.top, find_by_uname = 'svc_output_file')
  widget_control, val_fld, set_value = outfile
end

pro nrs_SavgolClean_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'svc_input_file')
  widget_control, val_fld, get_value = src_filename
  
  val_fld = widget_info(event.top, find_by_uname = 'svc_output_file')
  widget_control, val_fld, get_value = dest_filename
  
  val_fld = widget_info(event.top, find_by_uname = 'svc_width_field')
  widget_control, val_fld, get_value = width

  val_fld = widget_info(event.top, find_by_uname = 'svc_degree_field')
  widget_control, val_fld, get_value = degree

  val_fld = widget_info(event.top, find_by_uname = 'svc_order_field')
  widget_control, val_fld, get_value = order

  infile = strtrim(src_filename)
  outfile = strtrim(dest_filename)
  
  if strlen(infile) eq 0 then begin
    ans = dialog_message('No input file specified')
    return
  endif
  
  if strlen(outfile) eq 0 then begin
    ans = dialog_message('No output file specified')
    return
  endif
  
  val_fld = widget_info(event.top, find_by_uname = 'svc_field_list')
  indices = widget_info(val_fld, /list_select)

  envi_report_init, 'Using Savitzky-Golay filter to clean', base = rep, title = 'Progress' 
  csv_table = read_csv(infile, header = header, count = rec_count)
  out_header = header[indices]
  ind_cnt = n_elements(indices)
  
  ; do the cleanup
  out_table = dblarr(ind_cnt, rec_count)
  for col = 0, ind_cnt - 1 do begin
    envi_report_stat, rep, col, ind_cnt
    data = csv_table.(indices[col])
    nrs_clean_savgol, data, width = width, degree = degree, order = order
    
    out_table[col, *] = data
  endfor
  envi_report_stat, rep, ind_cnt, ind_cnt
  
  write_csv, outfile, out_table, header = out_header
  envi_report_init, base = rep, /finish 
  
  ans = dialog_message('Finished.', /information)
end
