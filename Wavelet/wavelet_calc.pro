; docformat = 'rst'
;+
; :Description:
;   Extract crystal
; :Params:
;   image: in, required
;     The image data with the wavelet coefficients
;   level: in required
;     The level of wavelet decomposition
;   index:
;     The details crystal index 
;
; :Author:
;   Willem Nieuwenhuis, 2010
; 
;-
function wav_get_crystal, image, level, index
  dd = size(image)
  columns = dd[1]
  
  determineCrystals, columns, level, crystals, names
  bounds = crystals[*, index]
  left  = bounds[0]
  top   = bounds[1]
  right = bounds[2]
  bottom = bounds[3]
  return, image[left:right, top:bottom]
end

;+
; :Description:
;   Normalize crystal by applying newvla = oldval * b + a
; :Params:
;   image: in, required
;     The image data with the wavelet coefficients
;   level: in required
;     The level of wavelet decomposition
;   index:
;     The details crystal index
;   a:
;     Offset normalization parameter
;   b:
;     Multiplication normalization parameter
;
; :Author:
;   Willem Nieuwenhuis, 2010
; 
;-
pro wav_normalize_crystal, image, level, index, a, b
  dd = size(image)
  columns = dd[1]
  
  determineCrystals, columns, level, crystals, names
  bounds = crystals[*, index]
  left  = bounds[0]
  top   = bounds[1]
  right = bounds[2]
  bottom = bounds[3]
  image[left:right, top:bottom] *= b + a
end

; docformat = 'rst'
;+
; :Description:
;   calculate the wavelet coefficients
; :Params:
;	  image: in, required
;     The image data
;	  coef_out: in, required
;	    The output coefficients image
;	  family: in, required
;	    The name of the wavelet family (Haar. Coifflet, etc)
;	  order: in, required
;	  		The order of the wavelet function
;	  levels: in, required
;	  		The maximum levels to calculate the wavelet for
;	  names: out, required
;	  		The list of crystal names at the deepest level
;	  crystals: out, required
;	  	The crystal boundaries
;
; :Author:
;   Willem Nieuwenhuis, 2010
;-
pro wavelet_calc_mem, image, coef_out, family, order, levels, names, crystals
	dd = size(image)
	lines = dd[2]
	columns = dd[1]

	func = 'wv_fn_' + family
	info = call_function(func, order, wavelet, scaling, ioff, joff)

  ; create the output array for the coefficients
  coef_out = fltarr(columns, lines)

  ; initialise tranquilizer
  envi_report_init,["Performing wavelet transformation",$
                      "This can take a few minutes"],$
            base = tranq, title = "progress"
  tranqstep = levels[0]
  envi_report_inc, tranq, tranqstep

  ; Now apply the wavelet levels
  for lv = 1, levels do begin
    if ptr_valid(coef) then ptr_free, coef
    ; get the coefficients for this level
    coef = ptr_new(wv_dwt(image, wavelet, scaling, ioff, joff, n_levels = lv), /no_copy)
    envi_report_stat, tranq, lv , levels[0], cancel = cancel

    len = columns
    determineCrystals, len, lv - 1, crystals, names
    num = n_elements(crystals) / 4
    bounds = crystals[*, num - 1]
    left  = bounds[0]
    top   = bounds[1]
    right = bounds[2]
    bottom = bounds[3]

    ; manual copy to prevent memory allocation
    ; Only decomposition of the previous smooth level is copied (IDL quirk:
    ; IDL decomposes the highpass decomposition as well, which is unwanted)
    for x = left, right do begin
      for y = top, bottom do begin
        coef_out[x, y] = (*coef)[x, y]
      endfor
    endfor

  endfor

  envi_report_init, base=tranq, /finish
end

;+
; :Description:
;   Perform inverse wavelet decomposition
; :Params:
;   image: in, required
;     The input coefficients
;   coef_out: in, required
;     The output coefficients image
;   family: in, required
;     The name of the wavelet family (Haar. Coifflet, etc)
;   order: in, required
;       The order of the wavelet function
;   levels: in, required
;       The level to transform back from
;
; :Author:
;   Willem Nieuwenhuis, 2010
;-
pro wavelet_calc_inv_mem, image, coef_out, family, order, levels
  nb = 1
  dd = size(image)
  lines = dd[2]
  columns = dd[1]
  if dd[0] gt 2 then nb = dd[3]

  func = 'wv_fn_' + family
  info = call_function(func, order, wavelet, scaling, ioff, joff)

  coef_out = fltarr(columns, lines, nb)
  ; Apply the wavelet levels
  for b = 0, nb - 1 do begin
    band = image[*,*,b]
    coef_out[*,*,b] = wv_dwt(band, wavelet, scaling, ioff, joff, n_levels = levels, /inverse)
  endfor
end

;+
; :Description:
;   calculate the wavelet coefficients
; :Params:
;   filename: in, required
;     The name of the image
;   coef_out: in, required
;     The name of the output coefficients (only for single file
;           processing, not used in batch mode)
;   folder: in, required
;       The name of the folder for the output
;   family: in, required
;       The name of the wavelet family (Haar. Coifflet, etc)
;   order: in, required
;      The order of the wavelet function
;   levels: in, required
;       The maximum levels to calculate the wavelet for
;   calcAll: in, required
;      If true calculate all intermediate levels also
;   names: out, required
;       The list of crystal names at the deepest level
;   inter: out, required
;       The wavelet coefficients at the deepest level
;   crystals: out, required
;    The crystal boundaries
;
; :Notes:
; If the 'folder' parameter is set to -1, the output will simply be generated
; in the input folder. Otherwise it contains the output folder
; In that case the output coefficient will be stored in files with
; names that are derived from the input file names
; 
; :Author:
;   Willem Nieuwenhuis, 2008
;-
pro wavelet_calc, filename, coef_out, folder, family, order, levels, calcAll, names, inter, crystals
  envi_open_file, filename, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then return  ; silently skip files that won't open

  ENVI_FILE_QUERY, fid, nl = lines, ns = columns
  dims = [-1, 0, columns - 1, 0, lines - 1]
  data = ENVI_GET_DATA(fid = fid, dims = dims, pos = 0)

  func = 'wv_fn_' + family
  info = call_function(func, order, wavelet, scaling, ioff, joff)

  outname = filename
  if folder eq '-1' then outname = coef_out
  ext = ''
  dot = strpos(outname, '.', /REVERSE_SEARCH)
  ; remove extension, if any
  if (dot ge 0) then begin
    ext = strmid(outname, dot)
    outname = strmid(outname, 0, dot)
  endif

  ; generate output file base name;
  ; remove existing path and prepend output folder
  if folder ne '-1' then begin
    slash = strpos(outname, '\', /REVERSE_SEARCH)
    if (slash ge 0) then begin
      name = strmid(outname, slash)
    endif
    outname = folder + name
  endif

  ; create the output array for the coefficients
  inter = fltarr(columns, lines)

  ; initialise tranquilizer
  envi_report_init,["Performing wavelet transformation",$
                      "This can take a few minutes"],$
            base = tranq, title = "progress"
  tranqstep = levels[0]
  envi_report_inc, tranq, tranqstep

  ; Now apply the wavelet levels
  for lv = 1, levels do begin
    if ptr_valid(coef) then ptr_free, coef
    ; get the coefficients for this level
    coef = ptr_new(wv_dwt(data, wavelet, scaling, ioff, joff, n_levels = lv), /no_copy)
    envi_report_stat, tranq, lv , levels[0], cancel = cancel

    len = columns
    determineCrystals, len, lv - 1, crystals, names
    num = n_elements(crystals) / 4
    bounds = crystals[*, num - 1]
    left  = bounds[0]
    top   = bounds[1]
    right = bounds[2]
    bottom = bounds[3]

    ; manual copy to prevent memory allocation
    ; Only decomposition of the previous smooth level is copied (IDL quirk:
    ; IDL decomposes the highpass decomposition as well, which is unwanted)
    for x = left, right do begin
      for y = top, bottom do begin
        inter[x, y] = (*coef)[x, y]
      endfor
    endfor

    ; only write the output for all levels if needed
    ; but always output the last level
    if (calcAll eq 1) or $
      ((calcAll ne 1) and (lv eq levels)) then begin

      ; and write the spectrum coefficients to disk
      fname = outname + string(lv, format='("_",I03)') + ext
      writeOutput, fname, inter, 1

      ; calculate the summary information and write to disk
      smyFilename = outname + string(lv, format='("_smy",I03)') + '.csv'
      smyQFilename = outname + string(lv, format='("_smy_Q",I03)') + '.csv'
      calcSummary_alt, inter, levels, lv, crystals, names, smyArray, smySubTotals
      writeSummary, smyArray, smySubtotals, names, smyFilename
      calcSummary_Q, inter, levels, lv, crystals, names, smyArray_Q, smySubTotals_Q
      writeSummary_Q, smyArray_Q, smySubtotals_Q, names, smyQFilename
    endif
  endfor

  envi_report_init, base=tranq, /finish
end

; inject low resolution image into high resolution DWT coefficients
pro wav_dwtInject, coefHigh, band, crystalsHigh
    num = n_elements(crystals) / 4
    bounds = crystals[*, num - 1] ; of lowpass crystal
    left  = bounds[0]
    top   = bounds[1]
    right = bounds[2]
    bottom = bounds[3]

    coefHigh[left:right, top:bottom] = band
end

