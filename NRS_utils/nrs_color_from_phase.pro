function nrs_color_from_phase, phase, radians = radians, red = r, green = g, blue = b
  compile_opt idl2, logical_predicate
  
  if n_elements(phase) eq 0 then return, 0
  
  useRadians = keyword_set(radians)
  
  p_loc = phase
  if useRadians then begin
    p_loc = phase * !radeg
  endif
  il = where(p_loc lt -180, l_cnt)
  ih = where(p_loc gt  180, h_cnt)
  ix = [il, ih]

  r = byte(((abs(p_loc) - 60) / 120.0 > 0) * 255)
  g = 255 - byte(abs((p_loc + 60.0) / 120.0 < 1) * 255)
  b = 255 - byte((abs(p_loc - 60.0) / 120.0 < 1) * 255)
  rgb = r * 256L * 256L + g * 256L + b
  
  if (l_cnt + h_cnt) gt 0 then begin
    r[ix] = 0
    b[ix] = 0
    g[ix] = 0
    rgb[ix] = 0L
  endif
  return, rgb
end

pro ttt
  phase = indgen(25) * 30 - 360
  cols = nrs_color_from_phase(phase, red = r, g = g, b = b)
  p = [transpose(phase),transpose(long(r)),transpose(long(g)),transpose(long(b)),transpose(cols)]
  print, p, format = '(5i)'
  
  phase = (indgen(25) * 30 - 360) * !dtor
  cols = nrs_color_from_phase(phase, red = r, g = g, b = b, /radians)
  p = [transpose(phase),transpose(long(r)),transpose(long(g)),transpose(long(b)),transpose(cols)]
  print, p, format = '(f8.2,4i)'
end
