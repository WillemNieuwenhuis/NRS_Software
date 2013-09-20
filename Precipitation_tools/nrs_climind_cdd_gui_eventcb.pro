pro nrs_rainfall_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_prix', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_rainfall_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_drylimit')
  widget_control, fld, get_value = dry
  
;  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_start_date')
;  widget_control, fld, get_value = sy
;  
;  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_end_date')
;  widget_control, fld, get_value = ey

  fld = widget_info(event.top, find_by_uname = 'nrs_rainfall_outputFile')
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

  nrs_rainfall_consecutive, infile $
                       , outname = outfile $
                       , /calcdry $
                       , /calcwet $
                       , /calchighwet $
                       , dry_limit = drylimit $
                       , high_limit = 20 $
;                       , start_date, end_date $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
