;+
; :Description:
;    Calculate a normalized weight window for a period of Ny years and a window size 0f N12 days
;    (See: ERA-Interim Daily Climatology, Martin Janoušek, ECMWF, january 2011)
;
;
;
; :Keywords:
;    N12 : in, optional, default = 30
;      Size of the window on either side of the center of the moving window
;    Ny : in, optional, default = 20
;      Number of years the weights will be applied in
;
; :Author: nieuwenhuis
; 
; :History:
;   - october 2019: nieuwenhuis, Created
;-
function nrs_weighted_determine_weights, N12 = N12, Ny = Ny
  compile_opt idl2, logical_predicate

  if n_elements(N12) eq 0 then N12 = 30
  
  if n_elements(Ny) eq 0 then Ny = 20
  
  factor = 3.0 * (N12 + 1) / (Ny * (2 * N12 + 1) * (2 * N12 + 3))
  
  weigths = factor * (1 - ( (findgen(2 * N12 + 1) - N12) / (N12 + 1) ) ^ 2)
  
  return, weights
end

;+
; :Description:
;    Calculate the weighted average on temporal samples (daily) by using a moving window
;    The weights are fixed and the window has size of 30 days around the center.
;    The dataset should therefore contain the values of interest extended with data 30 days
;    before and 30 days after.
;
; :Params:
;    samples : in, required
;      Daily temporal data (temperature, rainfall, etc)
;    weights : in, optional
;      If specified contains the (precalculated) weights for the moving window
;    outdata : out, optional
;      The data after being averaged
;
;
; :Author: nieuwenhuis
; 
; :History:
;   - october 2019: nieuwenhuis, Created 
;-
pro nrs_weighted_average, samples, weights = weights, outdata = outdata
  compile_opt idl2, logical_predicate

  if n_elements(weights) ne 61 then begin
    weights = nrs_weighted_determine_weights
  endif
  
  
end


pro nrs_climatology_test
  compile_opt idl2
  
end

pro nrs_climatology_test_weights
  compile_opt idl2, logical_predicate

  clim = nrsclimatology()
  clim.setproperty, n12 = 30, ny = 20
  clim.getproperty, weights = wght
  plot, wght
  
  clim.setproperty,n12 = 25
  clim.getproperty, weights = wght
  oplot, wght, color = 'ff00ff'x
  
end

function nrs_climatology_init_and_load_test
  compile_opt idl2, logical_predicate

  clim = nrsclimatology()
  clim.setproperty, base_folder = 'E:\Projects\ERA5_climatology\2m_temperature'
  clim.setproperty, file_mask = '2t*.tif'
  clim.setproperty, start_year = 1980, end_year = 1999, date_mask = '.yyyy.mm.dd'
  clim.setproperty, xstart = 750, xnum = 100, ystart = 150, ynum = 100
  clim.setproperty, n12 = 30
  
  clim.getproperty, datacube = data
  dims = size(data, /dim)
  print, dims     ; should still be empty
  
  clim.load_data
  
  return, clim
end

; test spatial subsetting
pro nrs_climatology_test_subset, clim, write = write
  compile_opt idl2, logical_predicate

  if ~obj_valid(clim) then begin
    void = error_message('Need to init climate object and load data first')
    return
  endif
  
  clim.getproperty, datacube = cube, julian = julian
  dims = size(cube, /dim)

  if keyword_set(write) then begin
    caldat, julian, mm, dd, yy
    format = '(i4,i02,i02)'
    bnames = string([transpose(yy),transpose(mm),transpose(dd)], format=format)
    envi_write_envi_file, cube, out_name = 'E:\Projects\ERA5_climatology\subcube.dat', ns = dims[0], nl = dims[1], bnames = bnames
  endif
end

pro nrs_climatology_test_stat_mean_var, clim, write = write
  compile_opt idl2, logical_predicate

  if ~obj_valid(clim) then begin
    void = error_message('Need to init climate object and load data first')
    return
  endif

  clim.statistics   ; mean and variance

  if keyword_set(write) then begin
    clim.getproperty, mean_data = mdata, var_data = vdata, dims = dims
    bnames = string(indgen(365) + 1, format = '("DOY: ",i03)')
    envi_write_envi_file, mdata, out_name = 'E:\Projects\ERA5_climatology\mean.dat', ns = dims[0], nl = dims[1], bnames = bnames
    envi_write_envi_file, vdata, out_name = 'E:\Projects\ERA5_climatology\var.dat', ns = dims[0], nl = dims[1], bnames = bnames
  endif

end

pro nrs_climatology_test_stat_quantile, clim, write = write
  compile_opt idl2, logical_predicate

  if ~obj_valid(clim) then begin
    void = error_message('Need to init climate object and load data first')
    return
  endif

  quantiles = [0, 0.25, 0.5, 0.75, 1.0]
  clim.quantiles, quantiles

  if keyword_set(write) then begin
    clim.getproperty, ny = ny, dims = dims
    days = reform(rebin(indgen(365) + 1, 365, n_elements(quantiles)),  365 * n_elements(quantiles)) 
    ques = reform(rebin(transpose(quantiles), 365, n_elements(quantiles)), 365 * n_elements(quantiles))
    form = '("DOY.Quantile: ",i03,".",f5.2)'
    bnames = string([transpose(days), transpose(ques)], format = form)
    clim.getproperty, quant_data = qdata
    envi_write_envi_file, qdata, out_name = 'E:\Projects\ERA5_climatology\quantiles.dat' $
      , bnames = bnames $
      , ns = dims[0], nl = dims[1], nb = ny * 365
  endif

end


pro nrs_climatology_run_tests
  compile_opt idl2, logical_predicate

  ; make sure to initiliase a climatology object and load test cube (because of time)
  print, 'Init object and load data (only subset)'
  tic   ; measure running time
  clim = nrs_climatology_init_and_load_test()
  toc   ; elapsed time

  print, 'test subsetting'
  tic   ; reset timer
  nrs_climatology_test_subset, clim, /write
  toc   ; elapsed time

  print, 'test mean, variance'
  tic   ; reset timer
  nrs_climatology_test_stat_mean_var, clim, /write
  toc   ; elapsed time

  print, 'test quantiles'
  tic   ; reset timer
  nrs_climatology_test_stat_quantile, clim, /write
  toc   ; elapsed time

   
end


