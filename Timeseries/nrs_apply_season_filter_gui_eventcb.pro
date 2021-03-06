function nrs_apply_season_filter_get_julian_dates, event
  compile_opt idl2, logical_predicate, hidden
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_start_date')
  widget_control, val_fld, get_value = str_sd
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_end_date')
  widget_control, val_fld, get_value = str_ed
  
  if strlen(strtrim(str_sd, 2)) eq 0 || strlen(strtrim(str_ed, 2)) eq 0 then return, []
  sd = nrs_str2julian(str_sd)
  ed = nrs_str2julian(str_ed)
  if n_elements(sd) + n_elements(ed) lt 2 then return, []
  if ed lt sd then begin
    temp = sd
    sd = ed
    ed = temp
  endif
  
  return, [sd, ed]
end

pro nrs_apply_season_filter_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_refimage')
  widget_control, val_fld, get_value = target
  
  target_str = strtrim(target, 2)
  if strlen(target_str) eq 0 then return
  
  basename = getoutname(target_str, postfix = '_grow', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_output')
  widget_control, val_fld, get_value = outfile
  outfile_str = strtrim(outfile, 2)
  if strlen(outfile_str) eq 0 then $
    widget_control, val_fld, set_value = basename
    
end

pro nrs_apply_season_filter_handlego, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_refimage')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_classes')
  widget_control, val_fld, get_value = climage
  climage = strtrim(climage, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_season_table')
  widget_control, val_fld, get_value = table
  table = strtrim(table, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_output')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_start_date')
  widget_control, val_fld, get_value = start_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_end_date')
  widget_control, val_fld, get_value = end_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_apply_season_filter_imgperyear')
  widget_control, val_fld, get_value = bands_py
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif
  
  range = nrs_apply_season_filter_get_julian_dates(event)
  if n_elements(range) lt 2 then begin
    void = error_message('Start and / or end date missing!', traceback = 0, /error)
    return
  endif
  
  if strlen(table) eq 0 then begin
    void = error_message('Input profile table not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif
  
  ; initialise tranquilizer
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Filter growing season" $
    , /fast_loop $
    )
    
  nrs_apply_season_filter, ref, climage, table $
    , start_date = start_date $
    , end_date = end_date $
    , outname = outname $
    , img_per_year = bands_py[0] $
    , prog_obj = prog_obj, cancelled = cancelled
    
  if obj_valid(prog_obj) then $
    prog_obj->destroy
end

