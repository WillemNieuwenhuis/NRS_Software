pro nrs_extract_time_handle_points, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_points')
  widget_control, fld, get_value = incsv

  incsv = strtrim(incsv, 2)
  if strlen(incsv) eq 0 then return
  
  outname = getOutname(incsv, postfix = '_ebt', ext = '.csv')

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_outputFile')
  widget_control, fld, set_value = outname

end

pro nrs_extract_time_handle_input, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
;  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_fieldname')
;  widget_control, fld, get_value = outfile
;  
;  outfile = strtrim(outfile, 2)
;  if strlen(outfile) gt 0 then return
;  
;  ext = nrs_get_file_extension(infile)
;  outfile = file_basename(infile, ext)
;  
;  widget_control, fld, set_value = outfile

  envi_open_file, infile, r_fid = fid, /no_realize, /no_interactive_query
  is_projected = fid ne -1
  
  if is_projected then begin
    mi = envi_get_map_info(fid = fid)
    is_projected = mi.proj[0].type ne 1
  endif
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_buffer_panel')
  widget_control, fld, sensitive = is_projected

end

pro nrs_extract_time_handleOK, event
  compile_opt idl2, logical_predicate
  
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_points')
  widget_control, fld, get_value = incsv

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_start_date')
  widget_control, fld, get_value = start

  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_end_date')
  widget_control, fld, get_value = finish

;  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_fieldname')
;  widget_control, fld, get_value = fieldname
;  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_outputFile')
  widget_control, fld, get_value = outfile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_buffer')
  widget_control, fld, get_value = buffer_str
  
;  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_buffer_panel')
;  do_buffer = widget_info(fld, /sensitive)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_extract_time_time_track')
  entire = ~widget_info(fld, /button_set)
  
  ; check input values
  incsv = strtrim(incsv, 2)
  infile = strtrim(infile, 2)
;  fieldname = strtrim(fieldname, 2)
  buffer_str = strtrim(buffer_str, 2)
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
  if n_elements(sd) gt 0 && n_elements(ed) gt 0 then begin
    if ed eq 0 || sd eq 0 then begin
      void = dialog_message('Missing date(s)', /error)
      return
    endif
    if ed lt sd then begin
      void = dialog_message('End date before start date', /error)
      return
    endif
  endif
  
  on_ioerror, NO_BUFFER 
  if strlen(buffer_str) gt 0 then begin
    buffer = float(buffer_str)
    goto, OK
  endif
  
  NO_BUFFER:  ; no warning when buffer is not a number
    void = temporary(buffer)  ; undefine buffer
  
  OK:

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
                   , buffer_dist = buffer $
                   , entire = entire $
                   , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
