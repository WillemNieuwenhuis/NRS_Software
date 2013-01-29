pro nrs_winter_ndvi_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_wnt', ext = '.')
  
  widget_control, fld, set_value = outfile
end

pro nrs_winter_ndvi_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_outputFile')
  widget_control, fld, get_value = outfile

  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_ipy')
  widget_control, fld, get_value = str_ipy
  
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_start_band')
  widget_control, fld, get_value = str_sb

  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_aggr_combo')
  aggr_method = widget_info(fld, /combobox_gettext)

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  ipy = fix(str_ipy[0])
  sb = fix(str_sb[0])
  
  ; start RPD calculation
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate winter NDVI' $
                        , /fast_loop $
                        )
  progressBar->Start

  nrs_winter_ndvi_beck, infile $
         , outfile $
         , images_per_year = ipy $
         , first = sb $
         , aggr_method = aggr_method $
         , prog_obj = progressBar, cancelled = cancelled

  if progressBar ne !null then $
    progressBar->Destroy
  
  fld = widget_info(event.top, find_by_uname = 'nrs_winter_ndvi_outputFile')
  widget_control, fld, set_value = ''
end
