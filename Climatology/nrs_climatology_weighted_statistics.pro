pro nrs_climatology_weighted_statistics, base_folder, file_mask = file_mask $
                     , output_folder = output_folder $
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
  clim.getproperty, mean_data = mdata, var_data = vdata, dims = dims
  bnames = string(indgen(365) + 1, format = '("DOY: ",i03)')
  meta = envirastermetadata()
  meta.AddItem, 'band names', bnames
  mras = enviraster(mdata, uri = mean_name, metadata = meta, interleave = 'bsq')
  mras.save
  mras.close 
  vras = enviraster(vdata, uri = var_name, metadata = meta, interleave = 'bsq')
  vras.save
  vras.close
  meta = 0
;  envi_write_envi_file, mdata, out_name = mean_name, ns = dims[0], nl = dims[1], bnames = bnames
;  envi_write_envi_file, vdata, out_name = var_name, ns = dims[0], nl = dims[1], bnames = bnames

  ; calculate the quantiles
  clim.quantiles, quantiles
  
  quant_name = output_folder + 'quantiles.dat'

  clim.getproperty, ny = ny, dims = dims
  days = reform(rebin(indgen(365) + 1, 365, n_elements(quantiles)),  365 * n_elements(quantiles))
  ques = reform(rebin(transpose(quantiles), 365, n_elements(quantiles)), 365 * n_elements(quantiles))
  form = '("DOY.Quantile: ",i03,"-",f-5.2)'
  bnames = string([transpose(days), transpose(ques)], format = form)
  clim.getproperty, quant_data = qdata

  qmeta = envirastermetadata()
  qmeta.AddItem, 'band names', bnames
  mras = enviraster(mdata, uri = quant_name, metadata = qmeta, interleave = 'bsq', ncolums = dims[0], nrows = dims[1], nbands = n_elements(quantiles) * 365)
  mras.save
  mras.close
;  envi_write_envi_file, qdata, out_name = quant_name $
;      , bnames = bnames $
;      , ns = dims[0], nl = dims[1], nb = n_elements(quantiles) * 365

end
