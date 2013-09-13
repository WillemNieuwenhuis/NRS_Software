pro nrs_climind_rnn_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_limit')
  widget_control, fld, get_value = limit
  
  postfix = '_rnn'
  if strlen(strtrim(limit, 2)) gt 0 then begin
    limit = fix(limit)
    postfix = string(limit, '("_",i02)')
  endif
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = postfix, ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_rnn_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_limit')
  widget_control, fld, get_value = limit
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_start_date')
  widget_control, fld, get_value = sd
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_end_date')
  widget_control, fld, get_value = ed

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_rnn_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  limit = fix(limit[0])
  
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
                        , ysize = 15, title = 'Calculate consecutive wet/dry days' $
                        , /fast_loop $
                        )
  nrs_climind_rnn, infile $
                       , outname = outfile $
                       , limit = limit $
                       , sd, ed $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->destroy
  
end
