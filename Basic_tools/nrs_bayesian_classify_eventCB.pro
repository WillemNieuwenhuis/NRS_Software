pro nrs_bayesian_classify_handle_locations, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_locations')
  widget_control, fld, get_value = in_table

  in_table = strtrim(in_table, 2)
  if strlen(in_table) eq 0 then return

  outname = getoutname(in_table, postfix = '_clf', ext = '.csv')

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_outtable')
  widget_control, fld, set_value = outname

end


pro nrs_bayesian_classify_handleOK, event
  compile_opt idl2, logical_predicate

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_locations')
  widget_control, fld, get_value = meas_table

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_conditional_folder')
  widget_control, fld, get_value = prob_folder

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_prior_table')
  widget_control, fld, get_value = prior_table

  fld = widget_info(event.top, find_by_uname = 'nrs_bayesian_classify_outtable')
  widget_control, fld, get_value = outtable

  evidence = strtrim(meas_table[0], 2)
  if strlen(evidence) eq 0 then begin
    void = error_message('No measurement table specified')
    return
  endif
  
  conditional = strtrim(prob_folder[0], 2)
  if strlen(conditional) eq 0 then conditional = temporary(void)
  
  prior = strtrim(prior_table[0], 2)
  if strlen(prior) eq 0 then prior = temporary(void)

  outname = strtrim(outtable[0], 2)
  if strlen(outname) eq 0 then outname = temporary(void)
  
;  prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green', ysize = 15 $
;    , title = 'Bayesian classification')
;  prog_obj -> Start
;
  nrs_bayesian_classify, evidence, conditional, prior = prior, outname = outname $
    , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) gt 0 then prog_obj->Destroy
end
