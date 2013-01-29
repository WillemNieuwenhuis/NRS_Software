;+
;
; :Description:
;   Calculate the simple linear regression between two data series, where
;   <pre>    img2 = slope * img1 + intercept</pre>
;   The dataseries are required to have the same length. Any 2D set is evaluated
;   as a 1D set: so if comparing 2 2D series the number of lines and columns should be equal
; 
; :Params:
;   img1 : in
;     The first data series
;   img2 : in
;     The second data series
;
; :Keywords:
;   slope : out
;     Gives the slope of the linear regression
;   intercept : out
;     Gives the intercept of the linear regression
;   R2 : out
;     Gives the R-squared value of the linear regression
;     
; :Author:
;   nieuwenhuis
;-
pro nrs_lin_regress, img1, img2, slope=slope, intercept=intercept, R2=R2
  len1 = n_elements(img1)
  len2 = n_elements(img2)
  if len1 ne len2 then return

  sx = total(img1, /double)
  sy = total(img2, /double)
  sxy = total(img1 * img2, /double)
  sx2 = total(img1 * img1, /double)  ; sum of squares
  sy2 = total(img2 * img2, /double)  ; sum of squares
  s2x = sx * sx ; square of sum
  s2y = sy * sy ; square of sum
  
  slope = (sxy - sx * sy / len1) / (sx2 - s2x / len1)
  intercept = (sy - slope * sx) / len1
  R2 = (len1 * sxy - sx * sy) ^ 2 / ((len1 * sx2 - s2x) * (len1 * sy2 - s2y))
end

;+
; :Description:
;   Calculate the feature space of two 2d-images.
;   The calculation will divide the value range of each image into several bins.
;   These bins can be specified in two ways;
;   <ul>
;   <li>with the <b>nrbins</b> keyword. All bins are equal in size and divided
;      over the entire value range of the images.
;   <li>with the <b>bins1</b> and <b>bins2</b> keywords. The bins are now determined 
;      by the values in the 1D-arrays. Each value marks a boundary between bins.
;   </ul>
;
;   The bins1 and bins2 keyword take precedence over the nrbins keyword. The bins1 and bins2
;   keywords will return the actually used bins in case the variables attached to them contain
;   less than 10 items.
; 
; :Params:
;   img1 : in
;     The first image (a 2D array)
;   img2 : in
;     The second image (a 2D array)
; 
; :Keywords:
;   nrbins : in
;     The number of value bins for each axis of the feature space (default = 255)
;   fs : output
;     The calculated feature space matrix (square)
;   bins1 : in
;     Array with the boundary values of the bins in image 1
;   bins2 : in
;     Array with the boundary values of the bins in image 2
;
; :Author:
;   nieuwenhuis
;-
pro nrs_calc_fs, img1, img2, nrbins = nrbins, fs = fs, bins1 = bins1, bins2 = bins2
  min1 = min(img1, max = max1)
  min2 = min(img2, max = max2)
  if not keyword_set(nrbins) then nrbins = 255
  hasBin1 = keyword_set(bins1) and (n_elements(bins1) gt 10)
  hasBin2 = keyword_set(bins2) and (n_elements(bins2) gt 10)
  
  if not hasBin1 then begin
    binsize = (max1 * 1.0 - min1) / (nrbins - 1)
    bins1 = findgen(nrbins) * binsize + min1
  end
  if not hasBin2 then begin
    binsize = (max2 * 1.0 - min2) / (nrbins - 1)
    bins2 = findgen(nrbins) * binsize + min2
  end
  ; allocate space for the feature space
  fs = lonarr(n_elements(bins1), n_elements(bins2))
  ; bin the values for both images
  lv1 = value_locate(bins1, img1)
  lv2 = value_locate(bins2, img2)
  ; fill the feature space
  fs[lv1,lv2]++
end

pro run_fs_linregr
  envi_select, fid = img1
  if img1 eq -1 then return
  envi_select, fid = img2
  if img2 eq -1 then return
  
  envi_file_query, img1, dims = dims1, fname = fname
  envi_file_query, img2, dims = dims2
  d1 = envi_get_data(fid = img1, dims = dims1, pos = [0])
  d2 = envi_get_data(fid = img2, dims = dims2, pos = [0])
  
  nrs_calc_fs, d1, d2, fs=fs
  nrs_lin_regress, d1, d2, slope = slope, intercept=intercept, R2=R2
  
  filename = getOutname(fname)
  envi_write_envi_file, fs, out_name = filename
end

pro nrs_lin_regress_test
  x = [1, 2.3, 3.1, 4.8, 5.6, 6.3]
  y = [2.6, 2.8, 3.1, 4.7, 5.1, 5.3]
  nrs_lin_regress, x, y, slope=slope, intercept=intercept, R2=R2
  
  ; expected:
  ; slope = 0.58418471
  ; intercept = 1.6842221
  ; R2 = 0.94878441
  print, 'Actual  : slope = ' + string(slope, format = '(f-0.4)') $
       + ', intercept = ' + string(intercept, format = '(f-0.4)') $
       + ', R2 = ' + string(R2, format = '(f-0.4)')
  print, 'Expected: slope = ' + string(0.58418471, format = '(f-0.4)') $
       + ', intercept = ' + string(1.6842221, format = '(f-0.4)') $
       + ', R2 = ' + string(0.94878441, format = '(f-0.4)')
  if (abs(slope - 0.58418471) lt 0.0001) $
    and (abs(intercept - 1.6842221) lt 0.0001) $
    and (abs(R2 - 0.94878441) lt 0.0001) then  print, 'Linear regression succesfull' $
  else print, 'Linear regression failed'
end

