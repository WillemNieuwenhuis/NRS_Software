function nrs_climatology_weighted_check_delete, fn, overwrite = overwrite
  compile_opt idl2, logical_predicate

  fi = file_info(fn)
  if fi.exists then begin
    if keyword_set(overwrite) then begin
      file_delete, fn
      file_delete, getoutname(fn, postfix = '', ext = '.hdr')
    endif $
    else begin
      void = error_message('Cannot overwrite existing file', /error)
      return, 0
    endelse
  endif

  return, 1
end

pro nrs_climatology_weighted_statistics, base_folder, file_mask = file_mask $
                     , output_folder = output_folder $
                     , overwrite = overwrite $
                     , start_year = start_year, end_year = end_year, date_pattern = date_pattern $
                     , n12 = n12 $
                     , quantiles = quantiles $
                     , xstart = xstart, xnum = xnum, ystart = ystart, ynum = ynum $
                     , prog_obj = prog_obj
  compile_opt idl2, logical_predicate

  if ~arg_present(base_folder) then begin
    void = error_message('Base input folder needs to be specified', /error)
    return
  endif
  
  if ~arg_present(output_folder) then begin
    void = error_message('Output folder needs to be specified', /error)
    return
  endif
  
  if strpos(output_folder, base_folder) ne -1 then begin
    void = error_message('Output folder cannot be the same or a subfolder of the input folder', /error)
    return
  endif

  if ~arg_present(start_year) || ~arg_present(end_year) then begin
    void = error_message('Both start and end year need to be specified', /error)
    return
  endif
  
  if ~arg_present(quantiles) then begin
    void = error_message('No quantiles specified, stopping', /error)
    return
  endif

  if ~arg_present(file_mask) then begin
    void = error_message('No file mask specified, using: "*.tif"', /information)
    file_mask = '*.tif'
  endif
  
  if ~arg_present(date_pattern) then begin
    void = error_message('No date pattern specified, using: ".yyyy.mm.dd"', /information)
    date_pattern = '.yyyy.mm.dd'
  endif

  if ~arg_present(n12) then begin
    void = error_message('No window size specified, using: 30 days', /information)
    n12 = 30
  endif

  ; make sure to end output folder with path separator
  if output_folder.lastIndexOf(path_sep()) ne (output_folder.strlen() - 1) then $
    output_folder = output_folder + path_sep()

  nrdays = 365
  clim = nrsclimatology()
  clim.setproperty, base_folder = base_folder
  clim.setproperty, file_mask = file_mask
  clim.setproperty, start_year = start_year, end_year = end_year, date_mask = date_pattern
  clim.setproperty, xstart = xstart, xnum = xnum, ystart = ystart, ynum = ynum
  clim.setproperty, n12 = n12
  
  if obj_valid(prog_obj) then $
    clim.setproperty, prog_obj = prog_obj

  ; load the data from the files in the basefolder (recursively)
  clim.load_data

  ; calculate weighted mean and variance, and store in the output folder as separate stacks
  clim.statistics

  mean_name = output_folder + 'mean.dat'
  var_name = output_folder + 'var.dat'
  min_name = output_folder + 'min.dat'
  max_name = output_folder + 'max.dat'
  ; remove previous files if existing and if allowed
  if ~nrs_climatology_weighted_check_delete(mean_name, overwrite = overwrite) then return
  if ~nrs_climatology_weighted_check_delete(var_name, overwrite = overwrite) then return
    
  clim.getproperty, mean_data = mdata, var_data = vdata, dims = dims
  bnames = string(indgen(nrdays) + 1, format = '("DOY: ",i03)')
  meta = envirastermetadata()
  meta.AddItem, 'band names', bnames
  mras = enviraster(mdata, uri = mean_name, metadata = meta, interleave = 'bsq')
  mras.save
  mras.close 
  vras = enviraster(vdata, uri = var_name, metadata = meta, interleave = 'bsq')
  vras.save
  vras.close
  meta = 0

  ; calculate the quantiles
  clim.quantiles, quantiles
  
  quant_basename = output_folder + 'quantile_'

  clim.getproperty, ny = ny, dims = dims
  clim.getproperty, quant_data = qdata, min_data = min_data, max_data = max_data

  qmeta = envirastermetadata()
  qmeta.AddItem, 'band names', bnames
  for q = 0, n_elements(quantiles) - 1 do begin
    quant_name = quant_basename + string(quantiles[q], format = '(f-4.2,".dat")')
    if ~nrs_climatology_weighted_check_delete(quant_name, overwrite = overwrite) then return
    mras = enviraster(qdata[*, *, *, q], uri = quant_name, metadata = qmeta, interleave = 'bsq', ncolumns = dims[0], nrows = dims[1], nbands = nrdays)
    mras.save
    mras.close
  endfor

  if ~nrs_climatology_weighted_check_delete(min_name, overwrite = overwrite) then return
  mnras = enviraster(min_data, uri = min_name, metadata = qmeta, interleave = 'bsq', ncolumns = dims[0], nrows = dims[1], nbands = nrdays)
  mnras.save
  mnras.close
  if ~nrs_climatology_weighted_check_delete(max_name, overwrite = overwrite) then return
  mxras = enviraster(max_data, uri = max_name, metadata = qmeta, interleave = 'bsq', ncolumns = dims[0], nrows = dims[1], nbands = nrdays)
  mxras.save
  mxras.close

end
