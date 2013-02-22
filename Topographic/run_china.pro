pro run_china
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