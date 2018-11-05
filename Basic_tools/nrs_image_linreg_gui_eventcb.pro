pro nrs_image_linreg_handle_input, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_image_linreg_refstack')
  widget_control, val_fld, get_value = obs

  obs = strtrim(obs, 2)
  if strlen(obs) eq 0 then return

  basename = getOutname(obs, postfix = '_coef', ext = '.dat')
  val_fld = widget_info(event.top, find_by_uname = 'nrs_image_linreg_outputFile')
  widget_control, val_fld, set_value = basename
end

pro nrs_image_linreg_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_image_linreg_refstack')
  widget_control, val_fld, get_value = obs
  obs = strtrim(obs, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_image_linreg_outputFile')
  widget_control, val_fld, get_value = basename

  val_fld = widget_info(event.top, find_by_uname = 'nrs_image_linreg_rmse')
  doRMSE = widget_info(val_fld, /button_set)

  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = "Calculate temporal linear regression" $
    , /fast_loop $
    )

  nrs_image_linreg, obs, output = basename $
    , rmse = doRMSE  $
    , prog_obj = progressBar, cancelled = cancelled

  if progressBar ne !null then $
    progressBar -> Destroy
end

