function NrsClimateIndices::init, _extra = ex
  compile_opt idl2

  ; call our superclass initialization method.
  void = self->idl_object::init()
  if (isa(ex)) then self->setproperty, _extra = ex
  self.percentile_95 = !values.f_nan
  self.percentile_99 = !values.f_nan
  
  return, 1
end

pro NrsClimateIndices::cleanup
  compile_opt idl2
  
  if ptr_valid(self.precip) then ptr_free, self.precip
  if ptr_valid(self.base) then ptr_free, self.base
  if obj_valid(self.prog_obj) then self.prog_obj->destroy
  
  ; call our superclass cleanup method
  self->idl_object::cleanup
end

pro NrsClimateIndices::r9xp_perc
  compile_opt idl2, logical_predicate

  nrs_set_progress_property, self.prog_obj, title = 'Calculating 95th percentile'

  if ptr_valid(self.base) then begin
    ; use independent timeseries to calculate the 95/99 percentile
    base = *(self.base)
  endif else begin
    dbase = *(self.precip)
    ; use the timeseries itself to calculate the 95/99 percentile
    ; make sure the base years are inside the full timeseries
    bsy = max([self.start_year, self.base_start_year])
    bey = min([self.end_year, self.base_end_year])
    nrs_get_dt_indices, [julday(1, 1, bsy), julday(12, 31, bey)], period = 'day', julian_out = jd_base
    jd_start = julday(1, 1, self.start_year)
    
    base_ix = jd_base - jd_start ; index of base years inside timeseries
    base = dbase[base_ix]
  endelse
  sel_ix = where(base gt 1, sel_cnt)  ; takes care of missing also (handles missing as NaN or missing lower than 0)
  prec = base[sel_ix]   ; only wet days

  prec = prec[sort(prec)]
  nr_prec = n_elements(prec)
  p95_ix = long(0.95 * nr_prec) ; percentile by nearest rank
  p99_ix = long(0.99 * nr_prec)
  self.percentile_95 = prec[p95_ix]
  self.percentile_99 = prec[p99_ix]

end

function NrsClimateIndices::rx5
  compile_opt idl2, logical_predicate

  precip = *(self.precip)
  
  nrs_get_dt_indices, [julday(1, 1, self.start_year), julday(12, 31, self.end_year)], period = 'day', julian_out = jd_base
  caldat, jd_base, mm, dd, yy
  
  ; prepare lookup table
  ; ix5 looks like:
  ;       0       1       2       3       4
  ;       1       2       3       4       5
  ;       2       3       4       5       6
  ;       3       4       5       6       7
  ;       etc
  np = 27   ; 27 = 31 - 5 + 1

  ix = transpose(reform(rebin(indgen(5), 5 * np), np, 5))
  ix2 = reform(rebin(indgen(np), 5 * np), 5, np)
  ix5 = ix + ix2
  
  ; per year / month
  nr_years = self.end_year - self.start_year + 1
  ym = (yy - yy.min())* 12 + mm - 1
  hym = histogram(ym, rev = ri, min = 0, nbins = ym.max() + 1)
  
  rx1 = fltarr(12 * nr_years)
  rx5 = fltarr(12 * nr_years)
  for i = 0, n_elements(hym) - 1 do begin
    pr = precip[ri[ri[i]:ri[i+1]-1]]  ; monthly data
    dat = pr[ix5[*, indgen(n_elements(pr) - 5)]]  ; 2D matrix with daily data shifted by one day per column
    
    rx1[i] = max(pr)
    rx5[i] = max(total(dat, 1))
  endfor

  year_agg = indgen(12 * nr_years) / 12 + self.start_year
  mon_agg = (indgen(12 * nr_years) mod 12) + 1
  out = create_struct('year', year_agg, 'month', mon_agg, 'rx1', rx1, 'rx5', rx5)
  return, out
end


function NrsClimateIndices::rnn, limit = limit
  compile_opt idl2, logical_predicate

  if n_elements(limit) eq 0 then limit = 10
  if limit lt 0 then limit = 10

  selected = *(self.precip) gt limit
     
  nrs_get_dt_indices, [julday(1, 1, self.start_year), julday(12, 31, self.end_year)], period = 'day', julian_out = jd_base
  caldat, jd_base, mon, day, year

  yyh = year - year.min()
  hy = histogram(yyh, rev = ri, min = 0, nbins = yyh.max() + 1)
  rnn = lonarr(n_elements(hy))
  for i = 0, n_elements(hy) - 1 do begin
    rnn[i] = total(selected[ri[ri[i]:ri[i+1]-1]]) 
  endfor

  uyears = year[uniq(year)]

  rnn_name = 'r' + string(limit, format = '(i0)')
  outdata = create_struct('year', uyears, rnn_name, rnn)
  
  return, outdata
  
end

;+
; :Description:
;
;-
function NrsClimateIndices::prcptot
  compile_opt idl2

  nrs_set_progress_property, self.prog_obj, /start

  nrs_set_progress_property, self.prog_obj, title = 'Calculating prcptot'
  
  nrs_get_dt_indices, [julday(1, 1, self.start_year), julday(12, 31, self.end_year)], period = 'day', julian_out = jd_base
  caldat, jd_base, mon, day, year

  nr_years = self.end_year - self.start_year + 1
  p_tot = fltarr(nr_years)

  timeseries = *(self.precip)
  for y = 0, nr_years - 1 do begin
    void = nrs_update_progress(self.prog_obj, y, nr_years)
    
    ydata = timeseries[where(y eq (year - self.start_year))]

    ixtot = where(ydata gt 1, ctot)

    p_tot[y] = total(ydata[ixtot])
  endfor

  uyears = year[uniq(year)]

  outdata = create_struct('year', uyears, 'prcptot', p_tot)   
  return, outdata
end

;+
; :Description:
;
;-
function NrsClimateIndices::r95ptot
  compile_opt idl2

  nrs_set_progress_property, self.prog_obj, /start

  ; calculate percentiles if not set
  if ~finite(self.percentile_95) then self->r9xp_perc
  
  nrs_set_progress_property, self.prog_obj, title = 'Calculating R95ptot'
  
  nrs_get_dt_indices, [julday(1, 1, self.start_year), julday(12, 31, self.end_year)], period = 'day', julian_out = jd_base
  caldat, jd_base, mon, day, year

  nr_years = self.end_year - self.start_year + 1
  r95_tot = fltarr(nr_years)
  r99_tot = fltarr(nr_years)

  timeseries = *(self.precip)
  for y = 0, nr_years - 1 do begin
    void = nrs_update_progress(self.prog_obj, y, nr_years)
    
    ydata = timeseries[where(y eq (year - self.start_year))]

    ix95 = where(ydata gt self.percentile_95, c95)
    ix99 = where(ydata gt self.percentile_99, c99)

    r95_tot[y] = total(ydata[ix95])
    r99_tot[y] = total(ydata[ix99])
  endfor

  uyears = year[uniq(year)]

  outdata = create_struct('year', uyears, 'r95ptot', r95_tot, 'r99ptot', r99_tot)   
  return, outdata
end


pro NrsClimateIndices::getproperty, start_year = start_year, end_year = end_year $
                                  , base_start_year = base_start_year, base_end_year = base_end_year $
                                  , perc_95 = perc_95, perc_99 = perc_99 $
                                  , precip = precip $
                                  , base_data = base_data
  compile_opt idl2

  if (isa(self)) then begin
    ; user asked for an "instance" property.
    if (arg_present(start_year)) then start_year = self.start_year
    if (arg_present(end_year)) then end_year = self.end_year
    if (arg_present(base_start_year)) then base_start_year = self.base_start_year
    if (arg_present(base_end_year)) then base_end_year = self.base_end_year
    if (arg_present(precip)) then begin
      if ptr_valid(self.precip) then precip = *(self.precip) else precip = []
    endif
    if (arg_present(base_data)) then begin
      if ptr_valid(self.base) then precip = *(self.base) else base_data = []
    endif
    if arg_present(perc_95) then perc_95 = self.percentile_95
    if arg_present(perc_99) then perc_99 = self.percentile_99
  endif
end

pro NrsClimateIndices::setproperty, start_year = start_year, end_year = end_year $
                                  , base_start_year = base_start_year, base_end_year = base_end_year $
                                  , precip = precip $
                                  , base_data = base_data $
                                  , prog_obj
  compile_opt idl2
  
  ; if user passed in a property, then set it.
  if (isa(start_year)) then self.start_year = start_year
  if (isa(end_year)) then self.end_year = end_year
  if (isa(base_start_year)) then self.base_start_year = base_start_year
  if (isa(base_end_year)) then self.base_end_year = base_end_year
  if (isa(precip)) then begin
    if ptr_valid(self.precip) then ptr_free, self.precip
    self.precip = ptr_new(precip)
  endif
  if (isa(base_precip)) then begin
    if ptr_valid(self.base) then ptr_free, self.base
    self.base = ptr_new(base)
  endif
  if isa(prog_obj, "PROGRESSBAR") then begin
    if obj_valid(self.prog_obj) then self.prog_obj->destroy
    self.prog_obj = prog_obj
  endif

  ; any property change will force recalculation of percentiles
  self.percentile_95 = !values.f_nan
  self.percentile_99 = !values.f_nan

end

pro NrsClimateIndices__define
  compile_opt idl2
  
  void = { NrsClimateIndices, $
    inherits idl_object $ ; superclass
    , precip:          ptr_new() $;!null $
    , base:            ptr_new() $;!null $
    , start_year:      0 $
    , end_year:        0 $
    , base_start_year: 0 $
    , base_end_year:   0 $
    , percentile_95:   -!values.f_nan $
    , percentile_99:   -!values.f_nan $
    , prog_obj:        obj_new() $;!null $
  }

end
