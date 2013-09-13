pro nrs_extract_time_handle_points, event
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_points')
  widget_control, fld, get_value = incsv

  incsv = strtrim(incsv, 2)
  if strlen(incsv) eq 0 then return
  
  outname = getOutname(incsv, postfix = '_ebt', ext = '.csv')

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_outputFile')
  widget_control, fld, set_value = outname

end

pro nrs_extract_time_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_fieldname')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  ext = nrs_get_file_extension(infile)
  outfile = file_basename(infile, ext)
  
  widget_control, fld, set_value = outfile
end

pro nrs_extract_time_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_points')
  widget_control, fld, get_value = incsv

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_start_date')
  widget_control, fld, get_value = start

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_end_date')
  widget_control, fld, get_value = finish

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_fieldname')
  widget_control, fld, get_value = fieldname
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  incsv = strtrim(incsv, 2)
  infile = strtrim(infile, 2)
  fieldname = strtrim(fieldname, 2)
  outfile = strtrim(outfile, 2)
  sd = nrs_str2julian(start)
  ed = nrs_str2julian(finish)
  
  if strlen(incsv) eq 0 then begin
    void = dialog_message('No input table specified', /error)
    return
  endif
  if strlen(infile) eq 0 then begin
    void = dialog_message('No input image specified', /error)
    return
  endif
  if ed eq 0 || sd eq 0 then begin
    void = dialog_message('Missing date(s)', /error)
    return
  endif
  if ed lt sd then begin
    void = dialog_message('End date before start date', /error)
    return
  endif

  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  ; start extraction
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Extract value by time' $
                        , /fast_loop $
                        )

  nrs_extract_by_time, incsv, infile, outname = outfile $
                   , start, finish $
                   , fieldname = fieldname $
                   , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
