pro nrs_savgol_row_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_input_csv')
  widget_control, val_fld, get_value = src_filename

  infile = strtrim(src_filename[0])
  if strlen(infile) eq 0 then return
  
  is_csv = query_csv(infile, info)
  
  if is_csv eq 0 then begin
    pos = strpos(infile, path_sep(), /reverse_search)
    if pos gt 0 then fname = strmid(infile, pos + 1)
    ans = message_dialog(fname + ' is not a CSV file; select another')
    return
  endif
  
  outfile = getoutname(infile, postfix = '_sgt', ext = '.csv')
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_outputFile')
  widget_control, val_fld, get_value = outname
  if strtrim(outname[0]) eq 0 then $
    widget_control, val_fld, set_value = outfile
end

pro nrs_savgol_row_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_input_csv')
  widget_control, val_fld, get_value = src_filename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_outputFile')
  widget_control, val_fld, get_value = dest_filename
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_width')
  widget_control, val_fld, get_value = width

  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_degree')
  widget_control, val_fld, get_value = degree

  val_fld = widget_info(event.top, find_by_uname = 'nrs_savgol_row_order_combo')
  order = widget_info(val_fld, /combobox_gettext)

  infile = strtrim(src_filename[0])
  outfile = strtrim(dest_filename[0])
  
  degree = fix(degree[0])
  width = fix(width[0])
  order = strtrim(order[0], 2)
  
  if strlen(infile) eq 0 then begin
    ans = dialog_message('No input file specified')
    return
  endif
  
  if strlen(outfile) eq 0 then begin
    ans = dialog_message('No output file specified')
    return
  endif
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Savitzky-Golay filter" $
                        , /fast_loop $
                        )
  
  nrs_set_progress_property, prog_obj, title = 'Savitzky-Golay filter', /start

  nrs_set_progress_property, prog_obj, text = 'Reading input table...', /start
  csv_table = nrs_read_csv(infile, header = header, count = rec_count)
  
  ind_cnt = n_elements(header) - 1 ; skip first column
  data = dblarr(ind_cnt)  ; row data array
  
  ; apply the filter
  out_table = dblarr(ind_cnt, rec_count)
  for row = 0, rec_count - 1 do begin
    if nrs_update_progress(prog_obj, row, rec_count, cancelled = cancelled) then return

    for c = 1, ind_cnt - 1 do data[c] = (csv_table.(c))[row]
    nrs_clean_savgol, data, width = width, degree = degree, order = order
    
    out_table[*, row] = data
    data[*] = 0.0
  endfor

  first_col = csv_table.(0)
  openw, unit, outfile, /get_lun
  printf, unit, strjoin(header, ',')
  for r = 0, rec_count - 1 do begin
    s = strjoin(string(out_table[*, r], format = '(f0.4)'), ',')
    printf, unit, first_col[r] + ',' + s
  endfor
  close, unit
  free_lun, unit
    
  if prog_obj ne !null then $
    prog_obj -> Destroy
  
  ans = dialog_message('Finished.', /information)
end
