function get_order_string, order
  if order eq 1 then begin
    return, 'first'
  endif
  if order eq 2 then begin
    return, '2nd'
  endif
  if order eq 3 then begin
    return, '3rd'
  endif
  
  return, string(order, format = '(i0)') + 'th'
end

pro Savgol_example, y1, x = x, width = width, degree = degree
 
  n = n_elements(y1) ; number of points  
  if n_elements(x) eq 0 then x = lindgen(n)
  if n_elements(width) eq 0 then width = 33
  if n_elements(degree) eq 0 then degree = 4
  hwidth = fix(width / 2)

  ; Display first plot 
  iPlot, x, y1, name = 'Signal + Noise';, view_grid = [1, 2] 
   
  ; Get an object reference to the iTool and insert legend. 
  void = itgetcurrent(tool = oTool) 
  void = oTool->DoAction('Operations/Insert/Legend') 
   
  ; Savitzky-Golay with user defined width and polynomial degree:  
  savgolFilter = savgol(hwidth, hwidth, 0, degree)
  name = 'Savitzky-Golay (width ' + string(width, format = '(i0)') + ', ' + get_order_string(degree) + ' degree)'
  iPlot, x, convol(y1, savgolFilter, /edge_truncate), /overplot, $ 
     color = [0, 0, 255], thick = 2, $ 
     name = name 
  void = oTool->DoAction('Operations/Insert/LegendItem') 
   
;  iPlot, x, deriv(x, deriv(x, y1)), yrange=[-4, 2], /view_next, $ 
;     name='second derivative' 
;   
;  void = oTool->DoAction('Operations/Insert/Legend') 
;   
;  dt = 0.1
;  order = 2 
;  ; Don't forget to normalize the coefficients. 
;  savgolFilter = savgol(16, 16, order, 4)*(factorial(order)/ $ 
;     (dt^order)) 
;  iPlot, x, convol(y1, savgolFilter, /edge_truncate), /overplot, $ 
;     color = [0, 0, 255], thick = 2, $ 
;     name='Savitzky-Golay(width 33, 4th degree, order 2)' 
;  void = oTool->DoAction('Operations/Insert/LegendItem') 
 
end 