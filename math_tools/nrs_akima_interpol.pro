pro ttt_akima
  m = findgen(7)+1
  print,m
  for i = 2, n_elements(m) - 2 do begin 
    num = abs(m[i + 1] - m[i]) * m[i - 1] + abs(m[i - 1] - m[i - 2]) * m[i]
    den = abs(m[i + 1] - m[i]) + abs(m[i - 1] - m[i - 2])
    print, num, '  ', den
  endfor

  format = '(10(i4))'
  print
  mt = [0, m, 0, 0]
  print, mt, format = format
  mm2 = shift(mt, 2)   ; m[i - 2]
  mm1 = shift(mt, 1)   ; m[i - 1]
  mp1 = shift(mt, -1)  ; m[i + 1]
  num = abs(mp1 - mt) * mm1 + abs(mm1 - mm2) * mt
  den = abs(mp1 - mt) + abs(mm1 - mm2)
  print, num[3:6]
  print, den[3:6]
end

;+
; :description:
;    Calculate the Akima spline
;
; :params:
;    x
;    y
;
; :keywords:
;    auto
;    lower_lim
;    nr_points
;
; :author: nieuwenhuis
; 
; :references:
;   Akima, H. (1970). "A New Method of Interpolation and Smooth Curve Fitting Based on Local Procedures." J. ACM 17(4): 589-602.
;-
pro nrs_akima_interpol, x, y, auto = xstep, lower_lim = lower_lim, nr_points = nr_points
  compile_opt idl2, logical_predicate
  
  if n_elements(xstep) eq 0 then xstep = 1.0  
  if n_elements(nr_point) eq 0 then interval = 100
  calc_limit = (n_elements(lower_lim) eq 0) || (n_elements(upper_lim) eq 0)
  if n_elements(lower_lim) eq 0 then lower_lim = 0.0
  if n_elements(upper_lim) eq 0 then upper_lim = 0.0

  if upper_lim lt lower_lim then begin
    temp = upper_lim
    upper_lim = lower_lim
    lower_lim = temp
  endif

  s = nrs_akima_spline(x, y, interval, xstep, lower_lim, upper_lim, calc_limit)
end

;+
; :description:
;    Calculate Akima spline
;
; :params:
;    x : in
;      x values; the values are assumed to be unique
;    y : in
;      y values
;
; :keywords:
;    interval  : in, optional
;      define the number of steps in output x values
;    lower_lim : in, optional
;      define the lower x value
;    upper_lim : in, optional
;      define the upper x value
;    calc_limit : in, optional
;      force the calculation of the x value limits
;
;
;
; :author: nieuwenhuis
;-
pro nrs_akima_spline, x, y $
                    , interval = interval, lower_lim = lower_lim, upper_lim = upper_lim, calc_limit = calc_limit $
                    , xout = xout, yout = yout
  compile_opt idl2, logical_predicate
  
  do_calc = keyword_set(calc_limit)
  lim_not_ok = (n_elements(upper_lim) eq 0) || n_elements(lower_lim eq 0)
  if !lim_not_ok then lim_not_ok = (upper_lim lt lower_lim)
  if do_calc || lim_not_ok then lower_lim = min(x, max = upper_lim)
  xstep = (upper_lim - lower_lim) / interval;
    
  nr_points = n_elements(x)
  if nr_points eq 1 then begin
    xout = x
    yout = y
  endif else if nr_points eq 2 then begin
    dx = x[1] - x[0]  ; x[1] <> x[0] assumed
    dy = y[1] - y[0]
    
    m = dy / dx
    xout = findgen(nr_points) * xstep + x[0]
    yout = findgen(nr_points) * xstep * m + y[0]
  endif else if nr_points gt 2 then begin
    x = [0.0, 0.0, temporary(x), 0.0, 0.0]
    y = [0.0, 0.0, temporary(y), 0.0, 0.0]
    
    n = n_elements(x)

    ; calculate slope
    mask = [0, 1, n - indgen(3) - 1]
    xxs = shift(x, -1)
    yys = shift(y, -1)
    dx = xxs - xx
    dy = yys - yy
    dx[mask] = 0.0  ; masks superfluous calculations 
    dy[mask] = 1.0  ; allow slope calculation without errors
    m = dy / dx

    ; extrapolate missing points 
    x[1] = x[2] + x[3] - x[4]
    y[1] = (x[2] - x[1]) * (m[3] - 2 * m[2]) + y[2]
    dx[1] = x[2] - x[1]
    dy[1] = y[2] - y[1]
    m[1] = dy[1] / dx[1]
    x[0] = 2 * x[2] - x[4]
    y[0] = (x[1] - x[0]) * (m[2] - 2 * m[1]) + y[1]
    dx[0] = x[1] - x[0]
    dy[0] = y[1] - y[0]
    m[0] = dy[0] / dx[0]
    
    x[n - 2] = x[n - 3] + x[n - 4] - x[n - 5]
    y[n - 2] = (2 * m[n - 4] - m[n - 5]) * (x[n - 2] - x[n - 3]) + y[n - 3]
    x[n - 1] = 2 * x[n - 3] - x[n - 5]
    y[n - 1] = (2 * m[n - 3] - m[n - 4]) * (x[n - 1] - x[n - 2]) + y[n - 2]
    
    t = dblarr(n)
    mt = [0, m, 0, 0]
    mm2 = shift(mt, 2)   ; m[i - 2]
    mm1 = shift(mt, 1)   ; m[i - 1]
    mp1 = shift(mt, -1)  ; m[i + 1]
    num = (abs(mp1 - m) * mm1 + abs(mm1 - mm2) * m)[1:-3]
    den = (abs(mp1 - m) + abs(mm1 - mm2))[1:-3]
    ix = where(den ne 0.0, cnt)
    if cnt gt 0 then t[ix] = num[ix] / den[ix]

    ; coefficients
    tp1 = shift([t, 0], -1)  ; t[i + 1]
    A = y
    B = t
    C = (3 * m - 2 * t - tp1) / dx
    D = (t + tp1 - 2 * m) / (dx ^ dx)

    ; Now interpolate
    xout = findgen(nr_points) * xstep + lower_lim
;;    /* 3rd step: output the coefficients for the subintervalls i=2..n-4 */
;    
;    p=2;
;    double xv;
;    for(xv=lower_lim; xv<upper_lim+xstep; xv += xstep) {
;      while (xv >= x[p]) {
;        printf("%g %g\n", x[p], y[p]); p++;
;      }
;    
;    
;      /* skip the next interpolated point if it's too close to the current point */
;      if (((xv - x[p-1]) > xstep/100.0) && 
;          ((x[p] - xv) > xstep/100.0)) {
;        double xd=(xv-x[p-1]);
;        printf("%g %g\n", xv,
;               y[p-1] + (t[p-1] + (C[p-1] + D[p-1]*xd)*xd)*xd);
;      }
;    }
;  
;    free(dx); free(dy); free(m); free(t); free(C); free(D);
;  }
  endif 
end