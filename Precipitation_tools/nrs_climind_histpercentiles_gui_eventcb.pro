pro nrs_climind_perc_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_perc_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_perc_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_hpct', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_climind_perc_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_perc_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_perc_button')
  zhang = widget_info(val_fld, /button_set)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_climind_perc_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Annual precipitation on very wet days' $
                        , /fast_loop $
                        )

  nrs_climind_percentiles, inname $
                       , outname = outfile $
                       , zhang = zhang $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->destroy
  
end
