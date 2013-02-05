pro nrs_aggregate_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_refstack')
  widget_control, val_fld, get_value = stackname

  stack_str = strtrim(stackname)
  if strlen(stack_str) eq 0 then return

  fld = widget_info(event.top, find_by_uname = 'nrs_aggregate_aggr_combo')
  aggr_method = widget_info(fld, /combobox_gettext)

  postfix = '_' + aggr_method
  if aggr_method eq 'All' then postfix = '_aggr'
  energy_name = getOutname(stack_str, postfix = postfix, ext = '.')
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
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Calculate aggreagtion" $
                        , /fast_loop $
                        )
  
  layers = indgen(nb)
  nrs_aggregate_layers, fid_ref, aggr_method, layers = layers, outname = outname, prog_obj = progressBar, cancelled = cancelled
  
  progressBar -> Destroy
end
