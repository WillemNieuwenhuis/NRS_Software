;+
; :Description:
;   Check the whether a pixel is within the bounds of the image
; 
; :Params:
;   x
;     X coordinate of pixel under investigation
;   y
;     Y coordinate of pixel under investigation
;   lines
;     number of lines in the image
;   columns
;     number of samples in the image
; :Returns:
; - 1 if the pixel is within the image
; - 0, if the pixel lies outside of the image
;-
function nrs_check_bounds, x, y, lines, columns
  if (x ge 0) and (x lt columns) and $
     (y ge 0) and (y lt lines) then return, 1
  return, 0
end

;+
; :Description:
;   Make sure a pixel is within the bounds of the image
; 
; :Params:
;   x: [in, out]
;     X coordinate of pixel under investigation
;   y: [in, out]
;     Y coordinate of pixel under investigation
;   lines: [in]
;     number of lines in the image
;   columns: [in]
;     number of samples in the image
; :Returns:
;   X coordinate is changed to be within [0, columns - 1]<br>
;   Y coordinate is changed to be within [0, lines - 1]
;-
pro nrs_crop_bounds, x, y, lines, columns
  if x lt 0 then x = 0
  if x gt columns then x = columns - 1
  if y lt 0 then y = 0
  if y gt lines then y = lines - 1
end

;+
; :Description:
; clip variables to a upper limit
; The input variable <b>_v</b> can be a scalar or an array
; The input limit array <b>vlim</b> can have any length
; The input variable <b>_v</b> is checked as long as there are elements in <b>vlim</b>
; 
; :Params:
;   _v
;     variables to clip
;   vlim
;     limits to clip the variables

; :Examples: ::
; 
;   Example 1
;   <code>
;      nrs_clip_to_bounds(50,10)<br>
;   </code> 
;    returns 9
;    
;   Example 2
;   <code>
;     nrs_clip_to_bounds([50,50,50],[100,10])<br>
;     nrs_clip_to_bounds([50,50,50],[100,10,100])<br>
;     nrs_clip_to_bounds([50,50,50],[100,10,99,99]<br>
;   </code>
;    All 3 return [50,9,50]<br>
;-
function nrs_clip_to_bounds, _v, vlim
  v = _v
  nr = n_elements(_v)
  nrl = n_elements(vlim)
  nrc = min([nr, nrl])
  for i = 0, nrc - 1 do begin
    if v[i] lt 0 then v[i] = 0
    if v[i] ge vlim[i] then v[i] = vlim[i] - 1
  endfor
  
  return, v
end
