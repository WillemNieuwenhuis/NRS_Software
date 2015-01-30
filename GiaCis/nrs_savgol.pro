function nrs_giacis_get_fit_window, i, ienvi, nb, win, wfit, yfit, win_thresh
  m1 = i - win[ienvi]
  m2 = i + win[ienvi] + 1
  winmax = max(win)
  
  ; Adapting fitting interval. Large variation use a smaller window.
  ymax = max(yfit[m1:m2 - 1], min = ymin)
  
  adjustWindow = (ymax - ymin) gt win_thresh
  if adjustWindow eq 1 then begin
    m1 = m1 + floor(win[ienvi] / 3) ; adjusting the left side with views of m1
    m2 = m2 - floor(win[ienvi] / 3) ; adjusting the right side with views of m2
  endif
  
  ; Check so that there are enough points, at least 3 at either side
  ; with weights different from zero. If not, extend fitting window
  failleft = 0
  while n_elements(where(abs(wfit[m1:i]) gt 1e-10)) lt 3 and failleft eq 0 do begin
    m1 = m1 - 1
    if m1 lt 1 then begin
      failleft = 1
      m1 = 1
    endif
  endwhile
  
  failright = 0
  while n_elements(where(abs(wfit[i:m2 - 1]) gt 1e-10)) lt 3 and failright eq 0 do begin
    m2 = m2 + 1
    if m2 gt nb + 2 * winmax then begin
      failright = 1
      m2 = nb + 2 * winmax
    endif
  endwhile
  
  return, [m1, m2, failleft, failright]
end

function nrs_giacis_savgol, y, w, win, forceUpperEnvelope, lastIterationLikeTIMESATfit
  ; Adapted code from TIMESAT
  winmax = max(win)
  nb = n_elements(y)
  
  ; Extend data circularity to improve fitting near the boundary of original data
  t = indgen(nb + 2 * winmax) - winmax + 1
  leftSlice = indgen(winmax) + nb - winmax
  rightSlice = indgen(winmax)
  
  yfit = [y[leftSlice], y, y[rightSlice]]
  wfit = [w[leftSlice], w, w[rightSlice]]
  
  ; general slice which points always to the profile data
  dataSlice = indgen(nb) + winmax
  
  nenvi = n_elements(win) ; number of fitting windows-nenvi
  yfits = fltarr(nb, nenvi)
  
  N = n_elements(yfit)
  bias = sqrt(1.0 * (N - 1) / N)
  for ienvi = 0, nenvi - 1 do begin
    ; Compute standard deviation for fitted function
    yfitstd = stddev(yfit[*]) * bias	; from unbiased stddev to biased (as in python version)
    win_thresh = 1.2 * 2 * yfitstd		; threshold to adjust the window
    for i = winmax, nb + winmax - 1 do begin
      ; set fitting window
      m1m2 = nrs_giacis_get_fit_window(i, ienvi, nb, win, wfit, yfit, win_thresh)
      m1 = m1m2[0]
      m2 = m1m2[1]
      failleft = m1m2[2]
      failright = m1m2[3]
      
      ; Fit the polynomial if enough data values with non-zero weight
      if failleft eq 0 and failright eq 0 then begin
        ; preparing data slices as to construct the design matrix
        s_wfit = wfit[m1:m2 - 1]
        s_t = t[0:m2 - m1 - 1]
        s_y = yfit[m1:m2 - 1]
        
        ; Construct the design matrix A and the column matrix b
        A = fltarr(3, n_elements(s_wfit))
        A[0, *] = s_wfit
        A[1, *] = s_wfit * s_t
        A[2, *] = s_wfit * s_t * s_t
        b = s_wfit * s_y
        
        ; Solving linear-squares problem A^TAc = A^Tb
        ATA = matrix_multiply(A, A, /btranspose)	; ATA = 3 x 3
        ATb = matrix_multiply(A, b)
        ludc, ATA, lu_index
        c = lusol(ATA, lu_index, ATb)
        
        ; Evaluating the fitted function
        yfit[i] = c[0] + c[1] * t[i - m1] + c[2] * t[i-m1] * t[i-m1]
      endif else begin
        s_y = yfit[m1:m2 - 1]
        yfit[i] = median(s_y)
      endelse
      
      if forceUpperEnvelope eq 1 then begin
        ; All iterations will be forced to the upper envelope
        if lastIterationLikeTIMESATfit eq 0 then begin
          if (yfit[i] lt y[i - winmax]) and wfit[i] eq 1 then yfit[i] = y[i - winmax]
        endif else begin
          ; All except the last iteration will be forced to the upper envelope
          if (yfit[i] lt y[i - winmax]) and (wfit[i] eq 1) and (ienvi lt nenvi - 1) then yfit[i] = y[i - winmax]
        endelse
      endif
      
    endfor
    yfits[*, ienvi] = yfit[dataSlice]
  endfor
  
  return, yfits
end
