pro nrs_climind_table, datafile, tbl_base = tbl_base, out_name = outname, indices = indices $
                     , base_start_year = base_start_year, base_end_year = base_end_year $
                     , limit = limit   ;; for rnn
  compile_opt idl2, logical_predicate

  void = nrs_climdex_load(datafile, data = mmp)
  clim = NrsClimateIndices()
  clim->setproperty, start_year = mmp.startyear, end_year = mmp.endyear, precip = mmp.precip
  if keyword_set(base_start_year) && keyword_set(base_end_year) then $
    clim->setproperty, base_start_year = base_start_year, base_end_year = base_end_year
  
  if n_elements(tbl_base) gt 0 then begin
    void = nrs_climdex_load(tbl_base, data = base_mmp)
    clim->setproperty, base_data = base_mmp.precip, base_start_year = base_mmp.startyear, base_end_year = base_mmp.endyear
  endif
  
  if n_elements(outname) eq 0 then outname = getoutname(table, postfix = '', ext = '.csv')
  
  index_list = ['rnn', 'r95ptot', 'rx5', 'prcptot']
  foreach index, index_list do begin
    i = where(index eq indices, icnt)
    if icnt eq 0 then continue
    
    outtbl = getoutname(outname, postfix = '_' + index, ext = '.csv')
    if index eq 'rnn' then begin
      if ~keyword_set(limit) then continue
      limit = fix(limit)
      if (limit lt 1) || (limit gt 100) then continue
      
      index_name = 'r' + string(limit, format = '(i0)')
      outdata = clim->rnn(limit = limit)
      write_csv, outtbl, outdata, header = ['year', index_name]
    endif
    
    if index eq 'r95ptot' then begin
      outdata = clim->r95ptot()
      write_csv, outtbl, outdata, header = ['year', 'r95ptot', 'r99ptot']
    endif
    
    if index eq 'rx5' then begin
      outdata = clim->rx5()
      write_csv, outtbl, outdata, header = ['year', 'month', 'rx1', 'rx5']
    endif
    
    if index eq 'prcptot' then begin
      outdata = clim->prcptot()
      write_csv, outtbl, outdata, header = ['year', 'prcptot']
    endif

  endforeach
  clim->destroy
end
