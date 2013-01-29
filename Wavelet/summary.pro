; parameters:
; 	ranges:	a list of ranges (left, top, right, bottom)
; 			top <= bottom, left <= right
function elementCount, ranges
	if n_elements(ranges) eq 0 then return, 0

	rows = n_elements(ranges[0, *])
	tot = 0L
	for row = 0, rows - 1 do begin
		line = ranges[*, row]
		xs = long(line[2] - line[0] + 1)
		ys = long(line[3] - line[1] + 1)
		tot += xs * ys
	end

	return, tot
end

;
; Copyright (c) 2007, ITC, NRS.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       SUMMARY
;
; PURPOSE:
;       This function computes several statistical parameters of
;       of an N-element vector. The returned result is a 8-element
;       vector containing o.a. the mean, variance of the input vector X.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = Summary(X)
;
; INPUTS:
;       X:      An N-element vector of type integer, float or double.
;
; EXAMPLE:
;       Define the N-element vector of sample data.
;         x = [65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71, 66, 65, 70]
;       Compute the mean, median, variance, MAD, SD, min, max, energy
;         result = summary(x)
;       The result should be the 8-element vector:
;       [66.7333, 67.0000, 7.06667, 2.15111, 2.65832, 62.0000, 71.0000, 258.648
;
;
; PROCEDURE:
;       SUMMARY computes the several statistics about  an N-element
;       vector of sample data. The computational formulas are given in the IDL
;       Reference Guide, Wikipedia, Numerical recipes in C
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; Modified: Tue Sep 11, 2007, Willem Nieuwenhuis, ITC
;	Mofified: Wed Dec 22, 2010, Willem Nieuwenhuis, ITC
;	  Added Q1, Q3
;-
; Returns:
;	8 element array containing:
;		[mean, median, variance, MAD, standard deviation, min, max, energy]
FUNCTION Summary, X, q1 = qtr_25th, q3 = qtr_75th

	ON_ERROR, 2

	IF N_ELEMENTS(x) lt 2 THEN $ ;Check length.
		return, [x, x, 0, 0, 0, x, x, x * x]

	sizeX = n_elements(X)
	meanVal = TOTAL(X) / sizeX
  ; Find the quartiles Q1, Q2 (median) and Q3
	medianVal = median(X, /even)
  qtr_25th = median(x[where(x le medianVal, countlowerhalf)], /even)
  qtr_75th = median(x[where(x gt medianVal, countupperhalf)], /even)
  
	minVal = min(X, max = maxVal)

	; calculate energy
	energy = total(x * x)

	; Calculate higher moments.
  resid = X - meanVal

	; Numerically-stable "two-pass" formula, which offers less
	; round-off error. Page 613, Numerical Recipes in C.
  var = (TOTAL(resid ^ 2) - (TOTAL(resid) ^ 2) / sizeX) / (sizeX - 1.0)
	;Mean absolute deviation (MAD).
	mad = TOTAL(ABS(resid)) / sizeX
	; Standard deviation (returned through the Sdev keyword).
  sdev = SQRT(var)

	RETURN, [meanVal, medianVal, var, mad, sdev, minVal, maxVal, energy]
END

; alternative implementation of the summary function that does not
; have a large memory footprint when executing: all calculations are done
; on the original values, only taking into account the ranges as specified
; this version does not calculate the median
; parameters:
; X   the actual data source
; ranges  a list of rectangle boundaries in the data source
; Returns:
; 8 element array containing:
;   [mean, median, variance, MAD, standard deviation, min, max, energy]
FUNCTION Summary_mem, X, ranges

  ON_ERROR, 2

  sizeX = elementCount(ranges)
  IF sizeX lt 2 THEN begin
    val = X[ranges[0,0]]
    return, [val, 0, 0, 0, 0, val, val, val * val]
  endif

  rows = n_elements(ranges[0, *])
  c = ranges[0, 0]
  l = ranges[1, 0]
  minVal = X[c, l]
  maxVal = X[c, l]
  tot = 0.0
  energy = 0.0
  ; first calculate total, energy and min and max
  for row = 0, rows - 1 do begin
    line = ranges[*, row]
    for l = line[1], line[3] do begin
      for c = line[0], line[2] do begin
        tot += X[c, l]
        energy += X[c, l] * X[c, l]
        if X[c, l] lt minVal then minVal = X[c, l]
        if X[c, l] gt maxVal then maxVal = X[c, l]
      endfor
    endfor
  endfor

  meanVal = tot / sizeX

  ; Then calculate residuals, variance and mean absolute deviation
  ; This uses an adaptation of the numerically-stable "two-pass" formula to calculate
  ; the variance, which offers less round-off error. Page 613, Numerical Recipes in C.
  resid_sum = 0.0
  resid_sum2 = 0.0
  var = 0.0
  mad = 0.0
  for row = 0, rows - 1 do begin
    line = ranges[*, row]
    for l = line[1], line[3] do begin
      for c = line[0], line[2] do begin
        resid = X[c, l] - meanVal
        resid_sum += resid
        resid_sum2 += resid * resid
        mad += abs(resid)
      endfor
    endfor
  endfor

  var = (resid_sum2 - (resid_sum * resid_sum / sizeX)) / (sizeX - 1.0)
  mad /= sizeX
  sdev = sqrt(var)

; medianVal = median(X)
  medianVal = 0.0 ; no result

  RETURN, [meanVal, medianVal, var, mad, sdev, minVal, maxVal, energy]
END

;+
; :Description:
;   alternative implementation of the summary function that does not
;   have a large memory footprint when executing: all calculations are done
;   on the original values as much as possible, only taking into account the
;   ranges as specified
;   This version does calculates the median and quartiles Q1 and Q3 and uses
;   these to calculate a threshold for outlier removal. The outliers are discarded
;   in the energy calculations
;   Q1_thresh = calculated as Q1 - (Q3 - Q1) * 1.5
;   Q3_thresh = calculated as Q3 + (Q3 - Q1) * 1.5
; :Params:
;	  X
;	    the actual data source
;   ranges
;     list of rectangle boundaries in the data source
; :Returns:
;	  10 element array containing:<br>
;    [mean, median, variance, MAD, standard deviation, min, max, energy, Q1_thresh, Q3_thresh]
;-
FUNCTION Summary_Q, X, ranges

	ON_ERROR, 2

	sizeX = elementCount(ranges)
	IF sizeX lt 2 THEN begin
		val = X[ranges[0,0]]
		return, [val, 0, 0, 0, 0, val, val, val * val]
	endif

	rows = n_elements(ranges[0, *])
	c = ranges[0, 0]
	l = ranges[1, 0]
	; first calculate Q1, Q2, Q3
	; get all needed data into single array
  for row = 0, rows - 1 do begin
    line = ranges[*, row]
    if n_elements(data) eq 0 then $
      data = x[line[0]:line[2], line[1]:line[3]] $
    else $ 
      data = [data, x[line[0]:line[2], line[1]:line[3]]]
  endfor
  medianVal = median(data)
  q1 = median(data[where(data le medianVal, countlowerhalf)], /even)
  q3 = median(data[where(data gt medianVal, countupperhalf)], /even)
  Q1_thresh = q1 - (q3 - q1) * 1.5
  Q3_thresh = q3 + (q3 - q1) * 1.5
  ix = where(data lt Q3_thresh)
  good =  data[ix]
  
  ; then calculate total, energy and min and max
  minVal = good[0, 0]
  maxVal = good[0, 0]
  tot = 0.0
  energy = 0.0
  tot = total(good)
  energy = total(good * good)
  minVal = min([minVal, maxVal, good], max = maxVal)
  
	meanVal = tot / sizeX

	; Then calculate residuals, variance and mean absolute deviation
	; This uses an adaptation of the numerically-stable "two-pass" formula to calculate
	; the variance, which offers less round-off error. Page 613, Numerical Recipes in C.
  resid = good - meanVal
  resid_sum = total(resid)
  resid_sum2 = total(resid * resid)
  mad = total(abs(resid))

	var = (resid_sum2 - (resid_sum * resid_sum / sizeX)) / (sizeX - 1.0)
	mad /= sizeX
	sdev = sqrt(var)

  return, [meanVal, medianVal, var, mad, sdev, minVal, maxVal, energy, Q1_thresh, Q3_thresh]
end

