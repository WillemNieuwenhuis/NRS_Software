function nrs_climind_table_history_select, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_history_option')
  widget_control, val_fld, get_value = hist_sel

  dates_fld = widget_info(event.top, find_by_uname='nrs_climind_table_basedates_base')
  table_fld = widget_info(event.top, find_by_uname='nrs_climind_table_basetable_base')

  widget_control, dates_fld, sensitive = (hist_sel eq 0)
  widget_control, table_fld, sensitive = (hist_sel eq 1)

  return, 0
end

pro nrs_climind_table_rnn_toggle, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_rnn_button')
  isOn = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname='nrs_climind_table_rnn_limit')

  widget_control, val_fld, sensitive = isOn

end

pro nrs_climind_table_handleOK, event
  compile_opt idl2, logical_predicate

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_obs')
  widget_control, val_fld, get_value = prec_table

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_history_option')
  widget_control, val_fld, get_value = hist_sel

  if hist_sel eq 0 then begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_base_startdate')
    widget_control, val_fld, get_value = start_year
    base_start_year = strtrim(start_year[0], 2)
  
    val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_base_enddate')
    widget_control, val_fld, get_value = end_year
    base_end_year = strtrim(end_year[0], 2)
  endif else begin
    val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_history_table')
    widget_control, val_fld, get_value = hist_table
    hist_table = strtrim(hist_table[0], 2)
  endelse

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_rnn_button')
  use_Rnn = widget_info(val_fld, /button_set)

  if use_Rnn then begin
    val_fld = widget_info(event.top, find_by_uname='nrs_climind_table_rnn_limit')
    widget_control, val_fld, get_value = limit
    limit = strtrim(limit[0], 2)
  endif

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_r95ptot_button')
  use_R95ptot = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_rx5_button')
  use_Rx5 = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_prcptot_button')
  use_prcptot = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_climind_table_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)

  indices = []
  if use_Rnn then indices = [indices, 'rnn']
  if use_R95ptot then indices = [indices, 'r95ptot']
  if use_Rx5 then indices = [indices, 'rx5']
  if use_prcptot then indices = [indices, 'prcptot']

  nrs_climind_table, prec_table, tbl_base = hist_table, out_name = outname, indices = indices $
    , base_start_year = base_start_year, base_end_year = base_end_year $
    , limit = limit
end

