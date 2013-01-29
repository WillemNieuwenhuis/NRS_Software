pro nrs_rpd_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_rpd_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_rpd_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_rpd', ext = '.')
  
  widget_control, fld, set_value = outfile
end

pro nrs_rpd_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_rpd_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_rpd_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  ; start RPD calculation
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate relative phenological development' $
                        , /fast_loop $
                        )
  progressBar->Start

  nrs_rpd, infile $
         , outname = outfile $
         , prog_obj = progressBar, cancelled = cancelled

  if progressBar ne !null then $
    progressBar->Destroy
  
  fld = widget_info(event.top, find_by_uname = 'nrs_rpd_outputFile')
  widget_control, fld, set_value = ''
end
