pro nrs_climind_r95p_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_r95p', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_r95p_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_percentile')
  widget_control, fld, get_value = hist95_file
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_start_date')
  widget_control, fld, get_value = sd
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_end_date')
  widget_control, fld, get_value = ed

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_r95p_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  hist95_file = strtrim(hist95_file, 2)
  if strlen(hist95_file) eq 0 then return
  
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
                        , ysize = 15, title = 'Annual precipitation on very wet days' $
                        , /fast_loop $
                        )
  nrs_climind_r95p, infile $
                  , hist95_file $
                  , outname = outfile $
                  , sd, ed $
                  , prog_obj = progressBar, cancelled = cancelled

  progressBar->destroy
  
end
