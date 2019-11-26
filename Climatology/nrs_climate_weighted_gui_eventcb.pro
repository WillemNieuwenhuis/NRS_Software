pro nrs_climate_weighted_subset_button_toggle, event
  compile_opt idl2

  if (event.id eq widget_info(event.top, find_by_uname='nrs_climate_weighted_subset_button')) then begin
    isOn = widget_info(event.id, /button_set)
    cpanel = widget_info(event.top, find_by_uname='nrs_climate_weighted_bounds_outer')
    widget_control, cpanel, sensitive = isOn
  endif
end

;pro nrs_climate_weighted_handle_input, event
;  compile_opt idl2
;
;  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_refstack')
;  widget_control, val_fld, get_value = stackname
;
;  stack_str = strtrim(stackname)
;  if strlen(stack_str) eq 0 then return
;
;  outname = getoutname(stack_str, postfix = '_pctl', ext = '.csv')
;
;  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_outputFile')
;  widget_control, val_fld, set_value = outname
;end

pro nrs_climate_weighted_handleok, event
  compile_opt idl2

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_basefolder')
  widget_control, val_fld, get_value = base_folder
  base_folder = strtrim(base_folder[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_file_pattern')
  widget_control, val_fld, get_value = file_pattern
  file_pattern = strtrim(file_pattern[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_date_pattern')
  widget_control, val_fld, get_value = date_pattern
  date_pattern = strtrim(date_pattern[0], 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_start_year')
  widget_control, val_fld, get_value = start_year
  start_year = fix(start_year[0])

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_end_year')
  widget_control, val_fld, get_value = end_year
  end_year = fix(end_year[0])

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_window')
  widget_control, val_fld, get_value = n12
  n12 = fix(n12[0])

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_percentiles')
  widget_control, val_fld, get_value = quantiles
  win_as = strsplit(quantiles, '[],',/extract)
  quantiles = float(win_as)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_subset_button')
  isOn = widget_info(event.id, /button_set)
  if isOn then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_bounds_xstart')
    widget_control, val_fld, get_value = xstart
    xstart = fix(xstart[0])

    val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_bounds_xnum')
    widget_control, val_fld, get_value = xnum
    xnum = fix(xnum[0])

    val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_bounds_ystart')
    widget_control, val_fld, get_value = ystart
    ystart = fix(ystart[0])

    val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_bounds_ynum')
    widget_control, val_fld, get_value = ynum
    ynum = fix(ynum[0])

  endif

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_overwrite_button')
  allowOverwrite = widget_info(event.id, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climate_weighted_outputfolder')
  widget_control, val_fld, get_value = output_folder
  output_folder = strtrim(output_folder[0], 2)

  ; initialise tranquilizer (will be destroted automatically after the calculations)
  progressBar = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Climatology statistics" $
    , /fast_loop $
    )

  nrs_climatology_weighted_statistics, base_folder, file_mask = file_pattern $
    , output_folder = output_folder $
    , overwrite = allowOverwrite $
    , start_year = start_year, end_year = end_year, date_pattern = date_pattern $
    , n12 = n12 $
    , xstart = xstart, xnum = xnum, ystart = ystart, ynum = ynum $
    , quantiles = quantiles $
    , prog_obj = progressBar

end
