pro nrs_growth_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_growth_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_growth_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_grow', ext = '.dat')
  
  widget_control, fld, set_value = outfile
end

pro nrs_growth_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_growth_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_growth_outputFile')
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
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate relative phenological development' $
                        , /fast_loop $
                        )

  nrs_find_growth_start, infile, outname = outfile, prog_obj = prog_obj, cancelled = cancelled

  if prog_obj ne !null then $
    prog_obj->Destroy
  
  fld = widget_info(event.top, find_by_uname = 'nrs_growth_outputFile')
  widget_control, fld, set_value = ''
end
