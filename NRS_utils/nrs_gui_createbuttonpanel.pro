pro nrs_gui_createButtonPanel, parent $
        , no_ok = no_ok $
        , no_cancel = no_cancel $
        , ok_value = ok_value, ok_tooltip = ok_tooltip, ok_uname = ok_uname, ok_enabled = ok_on $
        , cancel_value = cancel_value, cancel_tooltip = cancel_tooltip, cancel_uname = cancel_uname $
        , cancel_enabled = cancel_on

  compile_opt idl2, logical_predicate

  no_ok = keyword_set(no_ok)
  no_cancel = keyword_set(no_cancel)
  no_buttons = no_ok && no_cancel
  if no_buttons then return
   
  if n_elements(ok_uname) eq 0 then ok_uname = 'nrs_gui_buttonpanel_okbutton'
  if n_elements(ok_value) eq 0 then ok_value = 'Calculate'
  if n_elements(ok_tooltip) eq 0 then ok_tooltip = ''
  ok_on = (n_elements(ok_on) eq 0) or keyword_set(ok_on)
  if n_elements(cancel_uname) eq 0 then cancel_uname = 'nrs_gui_buttonpanel_cancelbutton'
  if n_elements(cancel_value) eq 0 then cancel_value = 'Cancel'
  if n_elements(cancel_tooltip) eq 0 then cancel_tooltip = ''
  cancel_on = (n_elements(cancel_on) eq 0) or keyword_set(cancel_on)
  ; Button panel (OK, Cancel etc)
  buttonpanel = widget_base(parent $
    , /align_right $
    , /row, space = 3 $
    , xpad = 3 $
    )

  if ~no_ok then $
    nrs_gui_gobutton = widget_button(buttonpanel, uname = ok_uname  $
;      , scr_xsize = 50, scr_ysize = 22  $
      , /align_center $
      , tooltip = ok_tooltip ,value = ok_value $
      , sensitiv = ok_on $
      )

  if ~no_cancel then $
    nrs_gui_cancelbutton = widget_button(buttonpanel, uname = cancel_uname  $
;      , scr_xsize = 50, scr_ysize = 22  $
      , /align_center $
      , tooltip = cancel_tooltip, value = cancel_value $
      , sensitiv = cancel_on $
    )

end

