pro nrs_climind_ptot_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  postfix = '_ptot'
  outfile = getoutname(infile, postfix = postfix, ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_ptot_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_start_date')
  widget_control, fld, get_value = sd
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_end_date')
  widget_control, fld, get_value = ed

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_ptot_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  if strlen(strtrim(sd, 2)) eq 0 then begin
    void = dialog_message('You need to specify the start date', /error)
    return
  endif
  
  if strlen(strtrim(ed, 2)) eq 0 then begin
    void = dialog_message('You need to specify the end date', /error)
    return
  endif
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  sd = nrs_str2julian(sd)
  ed = nrs_str2julian(ed)
  
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate annual wet day precipitation' $
                        , /fast_loop $
                        )
  nrs_climind_prcptot, infile $
                       , outname = outfile $
                       , sd, ed $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->destroy
  
end
