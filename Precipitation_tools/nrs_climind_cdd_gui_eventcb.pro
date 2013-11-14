pro nrs_climind_cdd_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_prix', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_cdd_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_drylimit')
  widget_control, fld, get_value = dry
  
;  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_start_date')
;  widget_control, fld, get_value = sy
;  
;  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_end_date')
;  widget_control, fld, get_value = ey

  fld = widget_info(event.top, find_by_uname = 'nrs_climind_cdd_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  drylimit = float(dry[0])
  
;  if strlen(strtrim(sy, 2)) eq 0 then begin
;    void = dialog_message('You need to specify the start year', /error)
;    return
;  endif
;  
;  if strlen(strtrim(ey, 2)) eq 0 then begin
;    void = dialog_message('You need to specify the end year', /error)
;    return
;  endif
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
;  ; start rainfall calculation
;  start_date = fix(sy[0])
;  end_date = fix(ey[0])
  
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate consecutive wet/dry days' $
                        , /fast_loop $
                        )
  progressBar->Start

  nrs_climind_cdd, infile $
                       , outname = outfile $
                       , /calcdry $
                       , /calcwet $
                       , dry_limit = drylimit $
;                       , start_date, end_date $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
