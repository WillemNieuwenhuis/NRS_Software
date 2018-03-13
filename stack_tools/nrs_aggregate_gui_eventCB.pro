pro nrs_aggregate_toggle_report, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_outliers_toggle')
  isOn = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_start_date')
  widget_control, val_fld, sensitiv = isOn

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_end_date')
  widget_control, val_fld, sensitiv = isOn
end

pro nrs_aggregate_toggle_exoutlier, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_usepert_toggle')
  isOn = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_outlier_base')
  widget_control, val_fld, sensitiv = isOn
end


pro nrs_aggregate_handle_input, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_refstack')
  widget_control, val_fld, get_value = stackname

  stack_str = strtrim(stackname)
  if strlen(stack_str) eq 0 then return

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_aggr_combo')
  aggr_method = widget_info(fld, /combobox_gettext)
  
  postfix = '_' + aggr_method
  if aggr_method eq 'All' then postfix = '_aggr'
  energy_name = getOutname(stack_str, postfix = postfix, ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_outputFile')
  widget_control, val_fld, set_value = energy_name
end

pro nrs_aggregate_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_refstack')
  widget_control, val_fld, get_value = ref

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_outputFile')
  widget_control, val_fld, get_value = outname

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_aggr_combo')
  aggr_method = widget_info(fld, /combobox_gettext)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_usepert_toggle')
  exclude_outliers = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_pertval_pert')
  widget_control, val_fld, get_value = pert_val
  pert_val = fix(pert_val[0])
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_start_date')
  widget_control, val_fld, get_value = start_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_end_date')
  widget_control, val_fld, get_value = end_date

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_outliers_toggle')
  report_outliers = widget_info(val_fld, /button_set)

  if strlen(strtrim(ref, 2)) eq 0 then begin
    void = error_message('Input stack not specified!', traceback = 0, /error)
    return
  endif
  
  envi_open_file, ref, r_fid = fid_ref, /no_realize, /no_interactive_query
  envi_file_query, fid_ref, nb = nb
  
  if fid_ref eq -1 then begin
    void = error_message('Input stack could not be opened!', traceback = 0, /error)
    return
  endif

    ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate aggregation" $
                        , /fast_loop $
                        )
  
  if exclude_outliers then begin
    if report_outliers then outlier_name = getOutname(outname, postfix = '_outlier', ext = '.dat')
    perc_low = pert_val
    perc_high = 100 - pert_val
    nrs_aggregate_excoutliers, stack = ref, outname = outname $
      , aggr_method = aggr_method $
      , perc_low = perc_low, perc_high = perc_high, exclude_outliers = exclude_outliers $
      , outlier_name = outlier_name $
      , start_date = start_date $
      , end_date = end_date $
      , prog_obj = prog_obj, cancelled = cancelled
  endif else begin
    layers = indgen(nb)
    nrs_aggregate_layers, fid_ref, aggr_method, layers = layers, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  
 endelse
  prog_obj -> Destroy
end
