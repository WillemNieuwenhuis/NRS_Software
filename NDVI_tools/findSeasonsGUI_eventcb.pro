pro fndSeason_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'fndSeason_inputImage')
  widget_control, fld, get_value = file
  
  if strlen(strtrim(file)) eq 0 then return
  
  outname = getoutname(file, ext = '.csv')
  
  fld = widget_info(event.top, find_by_uname = 'fndSeason_outputTable')
  widget_control, fld, set_value = outname
end

pro fndSeason_handleGo, Event
  fld = widget_info(event.top, find_by_uname = 'fndSeason_inputImage')
  widget_control, fld, get_value = imagefile

  fld = widget_info(event.top, find_by_uname = 'fndSeason_startDate')
  widget_control, fld, get_value = start_date

  fld = widget_info(event.top, find_by_uname = 'fndSeason_aggregatePeriod')
  widget_control, fld, get_value = aggr_period

  fld = widget_info(event.top, find_by_uname = 'fndSeason_Shapefile')
  widget_control, fld, get_value = shapefile

  fld = widget_info(event.top, find_by_uname = 'fndSeason_outputTable')
  widget_control, fld, get_value = outputfile
  
  if strtrim(imagefile, 2) eq '' then return
  if strtrim(start_date, 2) eq '' then return
  if strtrim(aggr_period, 2) eq '' then return
  if strtrim(shapefile, 2) eq '' then return
  if strtrim(outputfile, 2) eq '' then return
  
  start_date = fix(start_date, type = 3)
  aggr_period = fix(aggr_period)

  nrs_find_ndvi_seasons, imagefile, shapefile, outputfile, start_date = start_date, aggr_period = aggr_period
end
