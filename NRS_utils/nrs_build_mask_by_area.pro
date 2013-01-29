;+
; :Description:
;    Build a mask from the input layer. Values are masked if the area (in #pixels) 
;    of same-valued connected pixels is smaller than specified by the <i>area</i> keyword.
;    Pixels values are checked 4-connected (east, south, west, north)
;
; :Params:
;    layer
;      Input layer (2D) to build mask from; only integer values are supported (byte, int and uint)
;
; :Keywords:
;    area : in, default = 1
;      The maximum area in number of pixels for which pixels are masked.
;    mask : out
;      The calculated mask
;
; :Author: nieuwenhuis
;-
pro nrs_build_mask_by_area, layer, area = area, mask = mask, undef = undef, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 0

  ; checks
  if n_elements(area) eq 0 then area = 1
  dt = size(layer, /type)
  supported = [1, 2, 3, 12, 13]  ; byte, int, long, uint, ulong 
  t = supported - dt
  res = where(t eq 0, count)
  if count eq 0 then return

  ; calc
  rc = size(layer, /dim)  
  ns = rc[0]
  nl = rc[1]
  mask = bytarr(ns * nl) + 1
  min = min(layer, max = max)
  if n_elements(undef) eq 0 then undef = min - 1
  for cl = min, max do begin
    if (cl mod 100) eq 0 then begin
      if nrs_update_progress(prog_obj, cl - min, max - min, cancelled = cancelled) then return
    endif
    ix = where(layer eq cl, count)
    if count eq 0 then continue
    if (cl eq undef) or (count le area) then begin
      mask[ix] = 0
    endif
  endfor
end
