;+
; :Description:
;    Update the status of the progress indicator (ProgressBar object) and check if the user
;    aborts. The range is split into 1000 steps.
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
;    console : in, optional
;      When set print progress percentage to the console, when the progressBar is NULL.
;      The steps are displayed in 10% increments (more than 10 steps) or percentages rounded
;      to 10% increments (less than 10 steps)
;
; :Author: Willem
; 
;-
function nrs_update_progress, progressBar, pos, tot, cancelled = cancelled, console = console
  compile_opt idl2, logical_predicate
  
  cancelled = 0
  if n_elements(progressBar) eq 0 then begin
    if keyword_set(console) then begin
      vp1 = 10.0 * (pos) / tot
      vp2 = 10.0 * (pos + 1) / tot
      if (fix(vp2) - fix(vp1)) gt 0 then begin  
        print, 10 * fix(10.0 * (pos + 1) / tot), format = '(i0,"%")'
      endif
    endif
    
    return, cancelled
  endif
  
  vp1 = 1000.0 * (pos) / tot
  vp2 = 1000.0 * (pos + 1) / tot
  if (fix(vp2) - fix(vp1)) gt 0 then $
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

