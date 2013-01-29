pro nrs_monte_carlo_handle_input, event
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_input_table')
  widget_control, fld, get_value = infile
  
  target_str = strtrim(infile, 2)
  if strlen(target_str) eq 0 then return

  basename = getOutname(target_str, postfix = '_regr', ext = '.csv')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_monte_carlo_handleOK, event
  ; get input values
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_input_table')
  widget_control, fld, get_value = infile
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_outputFile')
  widget_control, fld, get_value = outfile

  ; get simulation  parameters
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_iteration')
  widget_control, fld, get_value = n_iter_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_seg_select')
  widget_control, fld, get_value = ess_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_acc_select')
  widget_control, fld, get_value = esa_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_iter_output_button')
  save_all_runs = widget_info(fld, /button_set)
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_carbon_field')
  widget_control, fld, get_value = cf_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_carbon_field_wrong')
  widget_control, fld, get_value = cfw_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_cpa_well')
  widget_control, fld, get_value = cpw_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_cpa_poor')
  widget_control, fld, get_value = cpp_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_height_well')
  widget_control, fld, get_value = hw_str
  
  fld = widget_info(event.top, find_by_uname = 'nrs_monte_carlo_height_poor')
  widget_control, fld, get_value = hp_str

  ; check input values
  infile = strtrim(infile, 2)
  if strlen(infile) eq 0 then return
  
  outfile = strtrim(outfile, 2)
  if strlen(outfile) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  n_iter = fix(n_iter_str)
  es_s = float(ess_str)
  es_a = float(esa_str)
  ec_s = float(cf_str)
  ec_w = float(cfw_str)
  ep_well = float(cpw_str)
  ep_poor = float(cpp_str)
  eh_well = float(hw_str)
  eh_poor = float(hp_str)
  
  ; start SPI classification
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Monte Carlo simulation for carbon' $
                        , /fast_loop $
                        )
  progressBar->Start

  nrs_monte_carlo, infile[0], outname = outfile[0] $
                 , n_iter[0] $
                 , save_all_runs = save_all_runs $
                 , es_s[0], es_a[0] $         ; number of samples (in %, for Segmentation, accuracy)
                 , ec_s[0], ec_w[0] $        ; Ecarbon (correct, wrongly classified)
                 , ep_well[0], ep_poor[0] $   ; Ecpa    (correct, wrongly classified)
                 , eh_well[0], eh_poor[0] $   ; Eheight (correct, wrongly classified) 
                 , prog_obj = progressBar, cancelled = cancelled

  progressBar->Destroy
  
end
