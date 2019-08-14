function nrs_payment_averages_get_julian_dates, event
  compile_opt idl2, logical_predicate, hidden

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_start_date')
  widget_control, val_fld, get_value = str_sd

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_end_date')
  widget_control, val_fld, get_value = str_ed

  if strlen(strtrim(str_sd, 2)) eq 0 || strlen(strtrim(str_ed, 2)) eq 0 then return, []
  sd = nrs_str2julian(str_sd)
  ed = nrs_str2julian(str_ed)
  if n_elements(sd) + n_elements(ed) lt 2 then return, []
  swapped = 0
  if ed lt sd then begin
    temp = sd
    sd = ed
    ed = temp
    swapped = 1
  endif

  return, [sd, ed, swapped]
end

pro nrs_payment_averages_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_refimage')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target, 2)
  if strlen(target_str) eq 0 then return

  basename = getoutname(target_str, postfix = '', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_output')
  widget_control, val_fld, get_value = outfile
  outfile_str = strtrim(outfile, 2)
  if strlen(outfile_str) eq 0 then $
    widget_control, val_fld, set_value = basename
end

pro nrs_payment_averages_early_season_toggle, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_early_season_button')
  isOn = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_early_table')
  widget_control, val_fld, sensitiv = isOn

end

pro nrs_payment_averages_late_season_toggle, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_late_season_button')
  isOn = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_late_table')
  widget_control, val_fld, sensitiv = isOn
end

pro nrs_payment_averages_long_season_toggle, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_long_season_button')
  isOn = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_long_table')
  widget_control, val_fld, sensitiv = isOn
end

pro nrs_payment_averages_calculation, ref, climage, table $
            , p5table, p25table $
            , start_date, end_date $
            , outbasename $
            , img_per_year $
            , retainGrow, retainPerc $
            , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate, hidden

  ; init: determine filenames for intermediairy products  
  grow = getOutname(outbasename, postfix = '_grow', ext = '.dat')
  perc = getOutname(outbasename, postfix = '_perc', ext = '.dat')
  outname = getoutname(outbasename, postfix = '_gra', ext = '.dat')

  ; Step 1: extract growing season data
  nrs_apply_season_filter, ref, climage, table $
    , start_date = start_date $
    , end_date = end_date $
    , outname = grow $
    , img_per_year = img_per_year $
    , prog_obj = prog_obj, cancelled = cancelled
  
  ; Step 2: Determine pay percentages
  nrs_interpolate_to_payment, ref, climage, p5table, p25table $
    , start_date = start_date $
    , end_date = end_date $
    , outname = perc $
    , img_per_year = img_per_year $
    , prog_obj = prog_obj, cancelled = cancelled
  
  ; Step 3: Calculate the average payment
  nrs_average_season_payment, ref, climage, table $
    , outname = outname $
    , start_date = start_date, end_date = end_date $
    , prog_obj = prog_obj, cancelled = cancelled
  
  ; Step 4: remove unneeded outputs
  if ~retainGrow then begin
    hdr = getOutname(grow, postfix = '', ext= '.hdr')
    dat = getOutname(grow, postfix = '', ext= '.dat')
    file_delete, hdr, /allow_nonexistent, /quiet
    file_delete, dat, /allow_nonexistent, /quiet
;    envi_open_file, grow, r_fid = r_fid, /no_realize, /no_interactive_query
;    if r_fid ne -1 then begin
;      envi_file_mng, id = r_fid, /delete
;    endif
  endif
  if ~retainPerc then begin
    hdr = getOutname(perc, postfix = '', ext= '.hdr')
    dat = getOutname(perc, postfix = '', ext= '.dat')
    file_delete, hdr, /allow_nonexistent, /quiet
    file_delete, dat, /allow_nonexistent, /quiet
;    envi_open_file, perc, r_fid = r_fid, /no_realize, /no_interactive_query
;    if r_fid ne -1 then begin
;      envi_file_mng, id = r_fid, /delete
;    endif
  endif

end

pro nrs_payment_averages_handlego, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_refimage')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_classes')
  widget_control, val_fld, get_value = climage
  climage = strtrim(climage, 2)

  early_table = ''
  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_early_season_button')
  doEarly = widget_info(val_fld, /button_set)
  if doEarly then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_early_table')
    widget_control, val_fld, get_value = early_table
    early_table = strtrim(early_table, 2)
  endif

  late_table = ''
  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_late_season_button')
  doLate = widget_info(val_fld, /button_set)
  if doLate then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_late_table')
    widget_control, val_fld, get_value = late_table
    late_table = strtrim(late_table, 2)
  endif

  long_table = ''
  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_long_season_button')
  doLong = widget_info(val_fld, /button_set)
  if doLong then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_long_table')
    widget_control, val_fld, get_value = long_table
    long_table = strtrim(long_table, 2)
  endif
    
  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_p5table')
  widget_control, val_fld, get_value = p5table
  p5table = strtrim(p5table, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_season_p25table')
  widget_control, val_fld, get_value = p25table
  p25table = strtrim(p5table, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_grow_season_button')
  retainGrow = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_percentages_button')
  retainPerc = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_output')
  widget_control, val_fld, get_value = basename
  basename = strtrim(basename, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_start_date')
  widget_control, val_fld, get_value = start_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_end_date')
  widget_control, val_fld, get_value = end_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_payment_averages_imgperyear')
  widget_control, val_fld, get_value = bands_py

  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif

  range = nrs_payment_averages_get_julian_dates(event)
  if n_elements(range) lt 2 then begin
    void = error_message('Start and / or end date missing!', traceback = 0, /error)
    return
  endif
  if range[2] eq 1 then begin  ; start_date later than end_date
    temp = start_date
    start_date = end_date
    end_date = temp
  endif

  if ~doEarly && ~doLate && ~doLong then begin
    void = error_message('At least one profile table must be specified!', traceback = 0, /error)
    return
  endif
  if doEarly && strlen(early_table) eq 0 then begin
    void = error_message('Early season table must be specified!', traceback = 0, /error)
    return
  endif
  if doLate && strlen(late_table) eq 0 then begin
    void = error_message('Late season table must be specified!', traceback = 0, /error)
    return
  endif
  if doLong && strlen(long_table) eq 0 then begin
    void = error_message('Long season table must be specified!', traceback = 0, /error)
    return
  endif

  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  if fid_ref eq -1 then begin
    void = error_message('Input timeseries could not be opened!', traceback = 0, /error)
    return
  endif

  ; initialise tranquilizer
  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , xsize = 250, ysize = 15, title = "Determine payment averages" $
    , /fast_loop $
    )

  img_per_year = bands_py[0]
  
  if doEarly then begin
    earlyoutname = getOutname(basename, postfix = '_early', ext = '.dat')
    nrs_payment_averages_calculation, ref, climage, early_table $
      , p5table, p25table $
      , start_date, end_date $
      , earlyoutname $
      , img_per_year $
      , retainGrow, retainPerc $
      , prog_obj = prog_obj, cancelled = cancelled
    
  endif

  if doLate then begin
    lateoutname = getOutname(basename, postfix = '_late')
    nrs_payment_averages_calculation, ref, climage, late_table $
      , p5table, p25table $
      , start_date, end_date $
      , lateoutname $
      , img_per_year $
      , retainGrow, retainPerc $
      , prog_obj = prog_obj, cancelled = cancelled
    
  endif
  if doLong then begin
    longoutname = getOutname(basename, postfix = '_long')
    nrs_payment_averages_calculation, ref, climage, long_table $
      , p5table, p25table $
      , start_date, end_date $
      , longoutname $
      , img_per_year $
      , retainGrow, retainPerc $
      , prog_obj = prog_obj, cancelled = cancelled
    
  endif

  if obj_valid(prog_obj) then $
    prog_obj->destroy
  
end

