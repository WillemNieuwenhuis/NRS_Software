pro nrs_aggregate_toggle_exoutlier, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_usepert_toggle')
  isOn = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_pertval_pert')
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

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_period_combo')
  period = widget_info(fld, /combobox_gettext)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_usepert_toggle')
  exclude_outliers = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_pertval_pert')
  widget_control, val_fld, get_value = pert_val
  pert_val = fix(pert_val[0])

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

  periods = ['8-day', '16-day', 'Monthly', 'Yearly']
  days = [8, 16, 30, 365]
  ix = where(period eq periods, cnt)
  if cnt eq 0 then begin
    void = error_message('Unsupported time period, stopping', /error)
    return
  endif
  period = days[ix]
  
    ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate aggregation" $
                        , /fast_loop $
                        )
  
  if exclude_outliers then begin
    perc_low = pert_val
    perc_high = 100 - pert_val
    aggregator = NrsStackAggregate()
    aggregator->setproperty, perc_low = perc_low, perc_high = perc_high, use_percentiles = exclude_outliers $
;      , base_start_year = base_start_year, base_end_year = base_end_year $
      , period = period $
      , stack_name = ref $
;      , outlier_name = outlier_name $
      , prog_obj = prog_obj
  endif else begin
    layers = indgen(nb)
    nrs_aggregate_layers, fid_ref, aggr_method, layers = layers, outname = outname, prog_obj = prog_obj, cancelled = cancelled
  
 endelse
  prog_obj -> Destroy
end
