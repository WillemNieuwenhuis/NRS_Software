;+
; :Description:
;    Update the status of the progress indicator (ProgressBar object) and check if the user
;    aborts.
;
; :Params:
;    progressBar : in
;      The progress indicator object; if it is undefined: do nothing
;    pos : in
;      Position in the calculation, must be less or equal to 'tot'
;    tot : in
;      Position of the end of the calculation
;
; :Keywords:
;    cancelled : out
;      Indicate if the user aborts
;
; :Author: Willem
;-
function nrs_update_progress, progressBar, pos, tot, cancelled = cancelled
  cancelled = 0
  if n_elements(progressBar) eq 0 then return, cancelled
  
  progressBar -> Update, 100.0 * (pos + 1) / tot, text = 'Progress: ' + string(pos + 1, format = '(i0)') + ' of ' + string(tot, format = '(i0)')
  cancelled = progressBar -> CheckCancel()
  if cancelled eq 1 then begin
    progressBar -> Destroy
    ans = dialog_message('Calculation interrupted by user', title = 'Information', /information)
  endif
  
  return, cancelled
end

pro nrs_set_progress_property, prog_obj, _extra = _extra, start = start, xs = xs, ys = ys
  if n_elements(prog_obj) gt 0 then begin
    if keyword_set(start) then prog_obj->Start, 0
    if n_elements(xs) ne 0 && n_elements(ys) ne 0 then $
      prog_obj->SetProperty, xoffset = xs, yoffset = ys

    if n_elements(_extra) gt 0 then $
      prog_obj->SetProperty, _extra = _extra
  endif
end