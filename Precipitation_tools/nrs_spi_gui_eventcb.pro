pro nrs_spi_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_refstack')
  widget_control, fld, get_value = infile
  
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_outputFile')
  widget_control, fld, get_value = outfile
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) gt 0 then return
  
  outfile = getoutname(infile, postfix = '_spi', ext = '.')
  
  widget_control, fld, set_value = outfile
end

pro nrs_spi_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_refstack')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_timescales')
  widget_control, fld, get_value = time_scales_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_start_year')
  widget_control, fld, get_value = sy
  
  fld = widget_info(event.top, find_by_uname = 'nrs_spi_end_year')
  widget_control, fld, get_value = ey

  fld = widget_info(event.top, find_by_uname = 'nrs_spi_outputFile')
  widget_control, fld, get_value = outfile

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  win_as = strsplit(time_scales_str, '[],', /extract)
  time_scales = fix(win_as)
  
  if strlen(strtrim(sy, 2)) eq 0 then begin
    void = dialog_message('You need to specify the start year', /error)
    return
  endif
  
  if strlen(strtrim(ey, 2)) eq 0 then begin
    void = dialog_message('You need to specify the end year', /error)
    return
  endif
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  ; start SPI calculation
  start_year = fix(sy[0])
  end_year = fix(ey[0])
  
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Calculate standardized precipitation index' $
                        , /fast_loop $
                        )
  progressBar->Start

  nrs_precipitation_index, infile $
                       , time_scales $
                       , outname = outfile $
                       , start_year, end_year $
                       , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
