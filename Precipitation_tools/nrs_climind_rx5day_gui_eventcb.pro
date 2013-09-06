pro nrs_climind_rx5day_handle_input, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_prix', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_rx5day_handleOK, event
  compile_opt idl2, logical_predicate
  
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_period')
  widget_control, fld, get_value = period_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_start_date')
  widget_control, fld, get_value = start
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_end_date')
  widget_control, fld, get_value = finish

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rx5day_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  period = (['month', 'year'])[fix(strtrim(period_str, 2))]
  
  if strlen(strtrim(start, 2)) eq 0 then begin
    void = dialog_message('You need to specify the start date', /error)
    return
  endif
  
  if strlen(strtrim(finish, 2)) eq 0 then begin
    void = dialog_message('You need to specify the end date', /error)
    return
  endif
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  ; start rainfall calculation
  sd = nrs_str2julian(start)
  ed = nrs_str2julian(finish)
  
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate consecutive wet/dry days' $
                        , /fast_loop $
                        )
  nrs_climind_rx5day, infile $
                       , outname = outfile $
                       , sd, ed $
                       , period = period $
                       , prog_obj = prog_obj, cancelled = cancelled

  prog_obj->Destroy
  
end
