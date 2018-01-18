pro nrs_import_netcdf_file_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_input_file')
  widget_control, val_fld, get_value = netcdf_file

  ; set / propose output name
  netcdf_file_str = strtrim(netcdf_file)
  if strlen(netcdf_file_str) eq 0 then return

  basename = getOutname(netcdf_file_str, postfix = '_var', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_outfile')
  widget_control, val_fld, set_value = basename

  ; fill listbox with variable names
  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_list')
  
  vars = nrs_nc_get_data_vars_from_file(netcdf_file_str)
  widget_control, val_fld, set_value = vars.name

  ; check for time variable
  ix = where(vars.t_id ge 0, tcnt)
  jdmin = []
  jdmax = []
  if tcnt gt 0 then begin ; at least one variable has time coordinates
    for v = 0, tcnt - 1 do begin
      var = vars[ix[v]]
      jd = nrs_nc_get_time_from_file(netcdf_file_str, var)
      jdmin = [jdmin, jd[0]]
      jdmax = [jdmax, jd[-1]]
    endfor
    jdmin = min(jdmin)
    jdmax = max(jdmax)
    start_date = nrs_julian_as_string(jdmin, format = 1)
    end_date = nrs_julian_as_string(jdmax, format = 1)
    val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_start_date')
    widget_control, val_fld, set_value = start_date
    val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_end_date')
    widget_control, val_fld, set_value = end_date
  endif
  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_dates_panel')
  widget_control, val_fld, sens = (tcnt gt 0) ? 1 : 0
end

pro nrs_import_netcdf_file_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_input_file')
  widget_control, val_fld, get_value = netcdf_file
  netcdf_file = strtrim(netcdf_file[0], 2)

  if strlen(netcdf_file) eq 0 then begin
    void = error_message('Input image not specified!', traceback = 0, /error)
    return
  endif

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_outfile')
  widget_control, val_fld, get_value = outname

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_list')
  var_index = widget_info(val_fld, /list_select)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_start_date')
  widget_control, val_fld, get_value = str_sd

  val_fld = widget_info(event.top, find_by_uname = 'nrs_import_netcdf_file_end_date')
  widget_control, val_fld, get_value = str_ed

  has_dates = (strlen(strtrim(str_sd, 2)) gt 0) && (strlen(strtrim(str_ed, 2)) gt 0)
  if has_dates then dates = [str_sd, str_ed]

  vars = nrs_nc_get_data_vars_from_file(netcdf_file)
  if var_index[0] eq -1 then $
    var_list = vars $
  else $
   var_list = vars[var_index]

  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Import NetCDF file' $
    , /fast_loop $
    )

  nrs_nc_get_data, netcdf_file, prog_obj = progressBar, cancelled = cancelled $
    , out_name = outname $
    , var_list = var_list $
    , date_range = dates

  if progressBar ne !null then $
    progressBar -> Destroy
end

