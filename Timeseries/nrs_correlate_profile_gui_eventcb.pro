function nrs_correlate_get_julian_dates, event
  compile_opt idl2, logical_predicate, hidden
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_start_date')
  widget_control, val_fld, get_value = str_sd

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_end_date')
  widget_control, val_fld, get_value = str_ed
  
  if strlen(strtrim(str_sd, 2)) eq 0 || strlen(strtrim(str_ed, 2)) eq 0 then return, []
  sd = nrs_str2julian(str_sd)
  ed = nrs_str2julian(str_ed)
  if n_elements(sd) + n_elements(ed) lt 2 then return, []
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_input_period_label')
  if ed lt sd then begin
    widget_control, val_fld, set_value = 'Input period: --'
    return, []
  endif

  return, [sd, ed]
end

pro nrs_correlate_profile_handle_input, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target, 2)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_corr', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_outputFile')
  widget_control, val_fld, get_value = outfile
  outfile_str = strtrim(outfile, 2)
  if strlen(outfile_str) eq 0 then $
    widget_control, val_fld, set_value = basename
  
  envi_open_file, target_str, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  envi_file_query, fid, nb = nb
  
  range = nrs_correlate_get_julian_dates(event)
  if n_elements(range) lt 2 then return
  
  per = nrs_get_period_from_range(range[0], range[1], nb, per_str = input_period)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_input_period_label')
  widget_control, val_fld, set_value = 'Input period: ' + input_period
end

pro nrs_correlate_profile_handle_from_to, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_refstack')
  widget_control, val_fld, get_value = target

  target_str = strtrim(target, 2)
  if strlen(target_str) eq 0 then return

  envi_open_file, target_str, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  envi_file_query, fid, nb = nb
  
  range = nrs_correlate_get_julian_dates(event)
  if n_elements(range) lt 2 then return

  widget_control, event.top, get_uvalue = info
  widget_control, event.id, get_uvalue = fromto
  
  fromto = strtrim(fromto, 2)
  if strlen(fromto) eq 0 then return
  
  if strpos(fromto, 'date') gt 0 then begin
    case fromto of
      'from_date' : begin
                      val = info.fromdate->get_Value()
                      target = info.fromband
                    end
      'to_date'   : begin
                      val = info.todate->get_Value()
                      target = info.toband
                    end
    endcase
    jd = nrs_str2julian(val)
    band = nrs_jul_to_band(jd, range[0], range[1], nb)
    target->set_Value, band
  endif
  
  if strpos(fromto, 'band') gt 0 then begin
    case fromto of
      'from_band' : begin
                      val = info.fromband->get_Value()
                      target = info.fromdate
                    end
      'to_band'   : begin
                      val = info.toband->get_Value()
                      target = info.todate
                    end
    endcase
    jd = nrs_band_to_jul(val, range[0], range[1], nb)
    nd = nrs_julian_as_string(jd, /form)
    target->set_Value, nd
  endif
end

pro nrs_correlate_profile_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_refstack')
  widget_control, val_fld, get_value = ref
  ref = strtrim(ref, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_table')
  widget_control, val_fld, get_value = table
  table = strtrim(table, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_outputFile')
  widget_control, val_fld, get_value = outname
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_start_date')
  widget_control, val_fld, get_value = start_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_end_date')
  widget_control, val_fld, get_value = end_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_NAN_button')
  ignore_NAN = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_from_date')
  widget_control, val_fld, get_value = from_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_to_date')
  widget_control, val_fld, get_value = to_date
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_from_band')
  widget_control, val_fld, get_value = from_band
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_correlate_profile_to_band')
  widget_control, val_fld, get_value = to_band
  
  if strlen(ref) eq 0 then begin
    void = error_message('Input timeseries not specified!', traceback = 0, /error)
    return
  endif

  range = nrs_correlate_get_julian_dates(event)
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
  
  envi_file_query, fid_ref, nb = nb
  
  fromtoOK = 1
  if n_elements(from_date) gt 0 && n_elements(to_date) gt 0 then begin
    dt = nrs_str2julian([from_date, to_date])
    if n_elements(dt) ne 2 then begin
      void = temporary(from_date)
      void = temporary(to_date)
      fromtoOK = 0
    endif
  endif
  if ~fromtoOK && n_elements(from_band) gt 0 && n_elements(to_band) gt 0 then begin
    sd = nrs_band_to_jul(from_band, range[0], range[1], nb)
    ed = nrs_band_to_jul(to_band, range[0], range[1], nb)
    from_date = nrs_julian_as_string(sd, format = 1)
    to_date = nrs_julian_as_string(ed, format = 1)
  endif
  
  ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Timeseries profile correlation" $
                        , /fast_loop $
                        )
  
  nrs_correlate_profile, ref, table $
                       , start_date = start_date $
                       , end_date = end_date $
                       , from_date = from_date $
                       , to_date = to_date $
                       , ignore_NAN = ignore_NAN $
                       , outname = outname $
                       , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $
    prog_obj->Destroy
end

