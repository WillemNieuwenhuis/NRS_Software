pro nrs_class_membership, image $
    , outname = outname $
    , kernel = kernel $
    , ignore_value = ignore_value $
    , cancelled = cancelled, prog_obj = prog_obj
  compile_opt idl2, logical_predicate

  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims, data_type = dt, data_ignore_value = nodata
  has_nodata = n_elements(nodata) gt 0
  if nb gt 1 then begin
    void = error_message('More than one band in the file, quitting', title = 'Class membership')
    return
  endif
  
  ; handle projection info
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  
  ; if the input is a tiff file copy the projection info from the input
  ext = nrs_get_file_extension(image)
  isTiff = (ext eq '.tif') || (ext eq '.tiff')
  if isTiff then begin
    isTiff = query_tiff(image, image_index = 0, geo = geokeys)
    ; geokeys = a struct in case success (type=8), or a long in case of failure (type = 3)
    csy_undef = (size(geokeys, /type) eq 3)
  endif else ext = '.dat'
  if csy_undef then begin
    delvar, mi
    delvar, geokeys
  endif

  ; base the base output filename on the input if not specified by the user
  if n_elements(outname) eq 0 then outname = image
  
  ; overrule ignore value if user specified
  if n_elements(ignore_value) gt 0 then nodata = ignote_value

  ; only positive kernels allowed, defaults to 3  
  if n_elements(kernel) le 0 then kernel = 3
  ; only odd kernel size allowed
  if (kernel mod 2) eq 0 then begin
    void = error_message('Only odd kernel sizes are allowed, quitting', title = 'Class membership')
    return
  endif
  if (kernel gt ns / 3) || (kernel ft nl / 3) eq 0 then begin
    void = error_message('Kernel size larger than 33% of the data, select a smaller kernel size', title = 'Class membership')
    return
  endif

  ; create moving window indices
  ver = rebin(transpose((indgen(kernel) - kernel / 2) * (ns + kernel - 1)), kernel, kernel)
  hor = rebin(indgen(kernel) - kernel / 2, kernel, kernel)
  win_index = ver + hor   ; relative indices wrt the center pixel

  ; create a data block extended with pixels on all sides to accomodate
  ; the moving window at the edges
  ; and fill with undefined values
  data = make_array(ns + kernel - 1, nl + kernel - 1, type = dt)
  
  ; read the data from file into the data block at the correct position
  kernel2 = kernel / 2
  filedata = envi_get_data(fid = fid, dims = dims, pos = 0)
  xx = where(filedata ne nodata, complement = xxundef)  ; find all the pixels that do not have undefined value
  minval = min(filedata[xx], max = maxval)  ; determine the amount of classes, excluding the undefineds
  filedata[xxundef] = maxval + 1  ; change undef value to unused clas to keep histogram result compact
  data[*] = maxval + 2    ; set edge to non existing class
  data[kernel2 : ns + kernel2 - 1, kernel2 : nl + kernel2 - 1] = filedata

  ; initialise progress indicator
  nrs_set_progress_property, prog_obj, /start, title = 'Class membership'

  ; create output cube
  out_data = fltarr(ns, nl, maxval - minval + 1)
  
  ; main loop
  data_ns = ns + kernel - 1
  data_nl = nl + kernel - 1 
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l + 1, nl, cancelled = cancelled) then return
    for c = 0, ns - 1 do begin
      data_l = l + kernel2
      data_c = c + kernel2
      index = win_index + data_l * data_ns  + data_c  ; make indices relative to current pixel
      block = data[index]
      h = histogram(block, min = minval, max = maxval + 2) ; we now have the counts for all classes, including the edge and undef classes
      h = h[0:-2]    ; remove the edge class; should not be included in membership calculations
      h = 100.0 * h / total(h)   ; turn into percentages
      out_data[c, l, *] = h[0:-2]   ; now save the percentages but leave out undef class
    endfor
  endfor

  ; write output to files  
  for f = minval, maxval do begin
    filename = getoutname(outname, postfix = string(f, format = '("_class", I0)'), ext = ext)
    if isTiff then begin
      write_tiff, filename, out_data[*,*, f - minval], /float, geotiff = geokeys
    endif else begin
      envi_write_envi_file, out_data[*,*,f - minval], out_name = filename, map_info = mi, /no_open
    endelse
  endfor
end
