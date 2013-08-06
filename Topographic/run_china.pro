pro run_china_month_1k
  demname = 'E:\NRS\Jiang Yanbing\Solar radiation model\PP\DEM_CN'
  
  envi_open_file, demname, r_fid = dem, /no_realize, /no_interactive_query
  
  if dem eq -1 then return
  
  jan = getoutname(demname, postfix = '_jan', ext = '.')
  feb = getoutname(demname, postfix = '_feb', ext = '.')
  mar = getoutname(demname, postfix = '_mar', ext = '.')
  apr = getoutname(demname, postfix = '_apr', ext = '.')
  may = getoutname(demname, postfix = '_may', ext = '.')
  jun = getoutname(demname, postfix = '_jun', ext = '.')
  jul = getoutname(demname, postfix = '_jul', ext = '.')
  aug = getoutname(demname, postfix = '_aug', ext = '.')
  sep = getoutname(demname, postfix = '_sep', ext = '.')
  oct = getoutname(demname, postfix = '_oct', ext = '.')
  nov = getoutname(demname, postfix = '_nov', ext = '.')
  dec = getoutname(demname, postfix = '_dec', ext = '.')
  ;run jan through dec
;  nrs_shortwaverad, dem, 1, 31, 60, output_name = jan ; januari
;  nrs_shortwaverad, dem, 32, 59, 60, output_name = feb ; februari
;  nrs_shortwaverad, dem, 60, 90, 60, output_name = mar ; march
;  nrs_shortwaverad, dem, 91, 120, 60, output_name = apr ; april
  nrs_shortwaverad, dem, 121, 151, 60, output_name = may ; may
  nrs_shortwaverad, dem, 152, 181, 60, output_name = jun ; june
  nrs_shortwaverad, dem, 182, 212, 60, output_name = jul ; july
  nrs_shortwaverad, dem, 213, 243, 60, output_name = aug ; august
  nrs_shortwaverad, dem, 244, 273, 60, output_name = sep ; september
;  nrs_shortwaverad, dem, 274, 304, 60, output_name = oct ; october
;  nrs_shortwaverad, dem, 305, 334, 60, output_name = nov ; november
;  nrs_shortwaverad, dem, 335, 365, 60, output_name = dec ; december
end

pro run_china_year_250, nobatch = nobatch
  compile_opt idl2, logical_predicate

  if ~keyword_set(nobatch) then begin
    envi, /restore_base_save_files
    envi_batch_init, log_file='E:\NRS\Fangyuan\DEM of China_250m\batch.txt', /no_status_window
  endif
  
  dems = [ $
            'E:\NRS\Fangyuan\sol250\China_DEM_mr.dat' $
;            'E:\NRS\Fangyuan\soltest2\subsub.dat' $
;            'E:\NRS\Fangyuan\solartest\china_dem_subset.dat' $
;           'E:\NRS\Fangyuan\DEM of China_250m\dem_tl_mr.dat' $
;         , 'E:\NRS\Fangyuan\DEM of China_250m\dem_tr_mr.dat' $
;         , 'E:\NRS\Fangyuan\DEM of China_250m\dem_bl_mr.dat' $
;         , 'E:\NRS\Fangyuan\DEM of China_250m\dem_br_mr.dat' $
         ]
  
  for i = 0, n_elements(dems) -1 do begin
    t1 = systime(/seconds)
    
    envi_open_file, dems[i], r_fid = dem, /no_realize, /no_interactive_query
  
    if dem eq -1 then begin
      print, 'Could not open ' + dems[i]
      continue
    endif
    print, 'Starting with: ' + dems[i]
  
;    name = getoutname(dems[i], postfix = '_jan', ext = '.dat')
;    nrs_solar_radiation, dem, 1, 31, 60, output_name = name
    name = getoutname(dems[i], postfix = '_feb', ext = '.dat')
    nrs_solar_radiation, dem, 32, 59, 60, output_name = name
    name = getoutname(dems[i], postfix = '_mar', ext = '.dat')
    nrs_solar_radiation, dem, 60, 90, 60, output_name = name
    name = getoutname(dems[i], postfix = '_apr', ext = '.dat')
    nrs_solar_radiation, dem, 91, 120, 60, output_name = name
    name = getoutname(dems[i], postfix = '_may', ext = '.dat')
    nrs_solar_radiation, dem, 121, 151, 60, output_name = name
    name = getoutname(dems[i], postfix = '_jun', ext = '.dat')
    nrs_solar_radiation, dem, 152, 181, 60, output_name = name
    name = getoutname(dems[i], postfix = '_jul', ext = '.dat')
    nrs_solar_radiation, dem, 182, 212, 60, output_name = name
    name = getoutname(dems[i], postfix = '_aug', ext = '.dat')
    nrs_solar_radiation, dem, 213, 243, 60, output_name = name
    name = getoutname(dems[i], postfix = '_sep', ext = '.dat')
    nrs_solar_radiation, dem, 244, 273, 60, output_name = name
    name = getoutname(dems[i], postfix = '_oct', ext = '.dat')
    nrs_solar_radiation, dem, 274, 304, 60, output_name = name
    name = getoutname(dems[i], postfix = '_nov', ext = '.dat')
    nrs_solar_radiation, dem, 305, 334, 60, output_name = name
    name = getoutname(dems[i], postfix = '_dec', ext = '.dat')
    nrs_solar_radiation, dem, 335, 365, 60, output_name = name
    
    envi_file_mng, id = dem, /remove
    print, 'Finished: ' + dems[i]
    print, 'Running time: ' + nrs_sec_to_string(systime(/seconds) - t1, /time, /hours24)
  endfor
  
  if ~keyword_set(nobatch) then begin
    envi_batch_exit
  endif
end

pro run_test
  fn = 'E:\NRS\Fangyuan\twi\yu\dem_mid.dat'
  envi_open_file, fn, r_fid = dem, /no_realize, /no_interactive_query
  t1 = systime(/seconds)

  name = getoutname(fn, postfix = '_rad', ext = '.dat')
  nrs_solar_radiation, dem, 1, 365, 60, output_name = name
  
  print, 'Finished: ' + fn
  print, 'Running time: ' + nrs_sec_to_string(systime(/seconds) - t1, /time, /hours24)
end

