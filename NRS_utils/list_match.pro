pro list_match, a, b, a_ind, b_ind
  flag = [replicate(0b, n_elements(a)), replicate(1b, n_elements(b))]
  s = [a, b]
  srt = bsort(s)
  s = s[srt]
  flag = flag[srt]
  wh = where(s eq shift(s, -1) and flag ne shift(flag, -1), cnt)
  if cnt ne 0 then begin
    a_ind = srt[wh]
    b_ind = srt[wh+1] - n_elements(a)
  endif else begin
    a_ind = -1 
    b_ind = -1 
    return
  endelse
end



