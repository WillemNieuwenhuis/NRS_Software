;*********************************************************
;  balance main procedure
;*********************************************************
; Purpose:
; Reflectance balancing based on geographic overlap in two images
; using multiple lineair regression (or polynominal fitting, uncomment/comment relevant sections)
; 
; Usage:
; Note, the user should modify:
; - imageNameX string (should point to correct input files)
; - preferred output of scatterplot (plots observed vs. simulated reflectance and super-imposes r2 of the correlation):
;   see nrs_plot2eps,filename, uncomment to output to eps instead of screen/comment to output to screen
; - corrected image is output to memory only at the moment. The user is automatically prompted by IDL/ENVI to store/display the ouput.
; 
; Output:
; -albedo (surface reflectance) of image2 is normalized to match image1

;+
; :Description:
;    Reflectance balancing calculated from the geographic overlap in two images
;    using multiple lineair regression (or polynominal fitting,
;    uncomment/comment relevant sections)
;
; :Params:
;    imageName1
;    imageName2
;
; :Keywords:
;    plotBounds
;    plotGraph
;    plot2File
;    verbose
;
; :Author: nieuwenhuis
; :History: Jan 10, 2013
;-
pro nrs_balance_images, imageName1, imageName2 $
                      , plotBounds = plotBounds $
                      , plotGraph = plotGraph $
                      , plot2File = plot2File $
                      , polyfit = polyfit $
                      , multifit = multifit $
                      , verbose = verbose
  compile_opt idl2, logical_predicate
  
  orgDevice = !D.Name ; save original plot device
  newDevice = 'PS'    ; switch to postscript output

	silent = 0
	
	polyfit = keyword_set(polyfit) && ~keyword_set(multifit)
	multifit = ~polyfit
	
  openImage, imageName1, imageID1, silent = silent
  openImage, imageName2, imageID2, silent = silent
  if (imageID1 eq -1) || (imageID2 eq -1) then begin
    print, 'Failed to open one or both images'
    return
  endif

  analyzingInterval = 10
  info1 = getImageInfo(imageID1)
  info2 = getImageInfo(imageID2)
  if (info1.mapY[0] gt info1.mapY[1]) then begin ; lat lon
    minX = max([info1.mapX[0], info2.mapX[0]])
    minY = max([info1.mapY[1], info2.mapY[1]])
    maxX = min([info1.mapX[1], info2.mapX[1]])
    maxY = min([info1.mapY[0], info2.mapY[0]])
  endif else begin  ; metric
    minX = max([info1.mapX[0], info2.mapX[0]])
    minY = max([info1.mapY[0], info2.mapY[0]])
    maxX = min([info1.mapX[1], info2.mapX[1]])
    maxY = min([info1.mapY[1], info2.mapY[1]])
  end
  overallMinX = min([info1.mapX, info2.mapX])
  overallMaxX = max([info1.mapX, info2.mapX])
  overallMinY = min([info1.mapY, info2.mapY])
  overallMaxY = max([info1.mapY, info2.mapY])

  NbX = (maxX - minX) / analyzingInterval + 2
  NbY = (maxY - minY) / analyzingInterval + 2

  if keyword_set(plotBounds) then begin
    plotXs1 = [info1.mapX[0], info1.mapX[0], info1.mapX[1], info1.mapX[1], info1.mapX[0]]
    plotYs1 = [info1.mapY[1], info1.mapY[0], info1.mapY[0], info1.mapY[1], info1.mapY[1]]
    plotXs2 = [info2.mapX[0], info2.mapX[0], info2.mapX[1], info2.mapX[1], info2.mapX[0]]
    plotYs2 = [info2.mapY[1], info2.mapY[0], info2.mapY[0], info2.mapY[1], info2.mapY[1]]
    plot, plotXs1, plotYs1 $
        , xrange = [overallMinX, overallMaxX] $
        , YRange = [overallMinY, overallMaxY]
    oplot, plotXs2, plotYs2
  endif
  
  ; now, construct arrays containing the row and column indices of the intersection
  startXs = findgen(NbX) * analyzingInterval + minX
  startYs = findgen(NbY) * analyzingInterval + minY
  I = intarr(NbY) +1
  allXs = I ## startXs
  I = intarr(NbX) + 1
  allYs = startYs ## I
  envi_convert_file_coordinates, imageID1, columns1, rows1, allXs, allYs
  envi_convert_file_coordinates, imageID2, columns2, rows2, allXs, allYs

  ; loop over all bands to correct them all
  NbBands = info2.NbBands
  rows = lindgen(info2.NbRows)
  columns = lindgen(info2.NbColumns)

  Ic = intarr(info2.NbColumns) + 1
  Ir = intarr(info2.NbRows) + 1
  allRows2 = rows ## Ic
  allColumns2 = Ir ## columns
  balancedData = fltarr(info2.NbColumns, info2.NbRows, NbBands)
  envi_convert_file_coordinates, imageID2, allColumns2, allRows2, allXs2, allYs2, /to_map
  
  for b = 1, NbBands do begin
    filename = getOutname(imageName2, postfix = 'band' + string(b, '(i0)'), ext = '.eps')
    if keyword_set(plot2File) then nrs_plot2eps, filename
  
    band1 = readSingleBand(imageID1, b - 1, silent = silent)
    band2 = readsingleBand(imageID2, b - 1, silent = silent)
    overlapData1 = band1[rows1, columns1]
    overlapData2 = band2[rows2, columns2]
    ; plot,overlapData1,overlapData2,PSYM=3

    mask = (overlapData1 GT 0 and overlapData1 LT 1) * (overlapData2 GT 0 and overlapData2 LT 1)

    ; extract co-ordinates and values using mask
    NbColumnsMask = (size(mask, /dimensions))[0]
    NbRowsMask    = (size(mask, /dimensions))[1]
    NbPixels = NbColumnsMask * NbRowsMask
    allXs1D = reform(allXs, NbPixels)
    allYs1D = reform(allYs, NbPixels)
    allXYs = allXs * allYs
    allXYs1D = reform(allXYs, NbPixels)
    mask1D = reform(mask, NbPixels)
    overlap11D = reform(overlapData1, NbPixels)
    overlap21D = reform(overlapData2, NbPixels)

    mix = where(mask gt 0, mix_cnt)
    if mix_cnt eq 0 then continue
    
    regressionXs = transpose([[allXs1D[mix]], [allYs1D[mix]], [overlap21D[mix]]])
    regressionXs1D = overlap21D[mix]
    regressionTargets = overlap11D[mix]

    if n_elements(regressionTargets) eq 1 then continue
    
    if multifit then begin
      ; perform multiple linear regression
      coeffs = regress(regressionXs, regressionTargets $
                        , correlation = correl $
                        , const = a0 $
                        , mcorrelation = r $
                        , yfit = y $
                      )
      predict = A0 + coeffs[0] * allXs1D[mix] + coeffs[1] * allYs1D[mix] + coeffs[2] * overlap21D[mix]
      r2 = r * r
      
      if keyword_set(verbose) then begin
        print, 'regression:', A0, coeffs
        print, 'correlation (x,y,DN):', correl
        print, 'band', b, ' coefficient of determination (r2):', r2
      endif
        
    endif
    
    if polyfit then begin
      ; perform quadratic fit
      A = [0.01, 0.01, 1.0, 0.01, 0.01]
      errors = 0.1 * regressionTargets
      coeffs = mpfitfun('balanceFunction', regressionXs, regressionTargets, errors, A)
      predict = balanceFunction(regressionXs, coeffs)
      correl = correlate(predict, regressionTargets)
      if keyword_set(verbose) then begin
        print, 'quadratic coeffs', coeffs
        print, 'quadratic fit', correl * correl
      endif
    endif

    plot, regressionTargets > 0, predict > 0, $
      psym = 3, $ ; print xy-data pairs as white dots
      color = white, $
      xtitle = 'Observed reflectance (%)', $ 
      ytitle = 'Simulated reflectance (%)' ;, $ 
      ;title ='Reflectance balancing based on overlap in two images'
    xyouts, 0.1, !y.crange[1] - 0.05, string('Band =', b)
    oplot, predict, Y, linestyle = 0, color = 255, thick = 3   ;overplot the fit as a dotted line
    if multifit then $
      xyouts, 0.1, !y.crange[1] - 0.1, string('R2    =', r2)
    oplot, !x.crange, !y.crange, linestyle = 5, color = black   ;overplot the 1:1 line as a dashed line
    xyouts, !x.crange[1] - 0.25, !y.crange[1] - 0.05, string('1:1 (dashed line)')
    
    set_plot, string(orgDevice); switch back to original plot device
    
    dataToBalance = band2[allRows2, allColumns2]
    balancedData[*, *, b - 1] = A0 + coeffs[0] * allXs2 + coeffs[1] * allYs2 + coeffs[2] * dataToBalance
    
    if keyword_set(verbose) then begin
      print,'Band done!'
    endif
  endfor

  ; display result in ENVI
  resID = arrayToENVI(balancedData, imageID2)

  ; close source images
  closeImage, imageId1, silent = silent
  closeImage, imageId2, silent = silent
end

;+
; :Description:
;    Plot to (E)PS file
;
; :Params:
;    fname : in, required
;      Filename for the EPS output
;
; :Author: nieuwenhuis
; :History: Jan 10, 2013
;-
pro nrs_plot2eps, fname
  compile_opt idl2
  
  if n_elements(fname) gt 0 && strlen(fname) gt 0 then begin
    device, $
      xsize = 15, $
      ysize = 15, $
      encapsulated = 1, $
      language_level = 2, $
      decomposed = 1, $
      color = 1, $
      bits_per_pixel = 8, $
      /isolatin1, $
      filename = fname
      
  endif
end

;+
; :Description:
;    Apply the coefficients from the fitting function to balance the data.
;
; :Returns:
;    The balanced data
;
; :Params:
;    x : in, required
;      Variables
;    a : in, required
;      Coefficients
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function balanceFunction, x, a
  compile_opt idl2
  
	res = a[0] + a[1] * x[0, *] + a[2] * x[1, *] + a[3] * x[2, *] + a[4] * x[2, *] * x[2, *]
	
	return, res
end

;+
; :Description:
;    Apply the coefficients from the fitting function to balance the data.
;
; :Returns:
;    The balanced data 
;    
; :Params:
;    xs : in, required
;      X-coordinates
;    ys : in, required
;      Y-coordinates
;    dns : in, required
;      Unbalanced data
;    a : in, required
;      Coefficients
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function processBalanceFunction, xs, ys, dns, a
  compile_opt idl2
  
	res = a[0] + a[1] * xs + a[2] * ys + a[3] * dns + a[4] * dns * dns
	
	return,res
end

pro testExport
  compile_opt idl2, hidden
  
	silent = 0
	imageName = 'FILE-LOCATION'
	openImage, imageName, imageID, silent = silent
	band1 = readSingleBand(imageID, 0)
	resID = arrayToENVI(band1, imageID)
  closeImage, imageID
END