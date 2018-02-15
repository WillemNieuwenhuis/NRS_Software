function NrsStackAggregate::init, _extra = ex
  compile_opt idl2

  ; call our superclass initialization method.
  void = self->idl_object::init()
  if (isa(ex)) then self->setproperty, _extra = ex
  self.p_low = !values.f_nan
  self.p_high = !values.f_nan

  return, 1
end

pro NrsStackAggregate::cleanup
  compile_opt idl2

  if ptr_valid(self.precipitation_low) then ptr_free, self.precipitation_low
  if ptr_valid(self.precipitation_high) then ptr_free, self.precipitation_high
;  if ptr_valid(self.base) then ptr_free, self.base
;  if obj_valid(self.prog_obj) then self.prog_obj->destroy

  ; call our superclass cleanup method
  self->idl_object::cleanup
end

pro NrsStackAggregate::getproperty ;$ ;, start_year = start_year, end_year = end_year $
;  , base_start_year = base_start_year, base_end_year = base_end_year $
;  , perc_95 = perc_95, perc_99 = perc_99 $
;  , precip = precip $
;  , base_data = base_data
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

pro NrsStackAggregate::setproperty, perc_low = perc_low, perc_high = perc_high $
;  , base_start_year = base_start_year, base_end_year = base_end_year $
;  , precip = precip $
;  , base_data = base_data $
  , prog_obj
  compile_opt idl2

  ; if user passed in a property, then set it.
  if (isa(perc_low)) then self.p_low = perc_low
  if (isa(perc_high)) then self.p_high = perc_high
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

pro NrsStackAggregate::calculate_percentiles
  compile_opt idl2, logical_predicate

  nrs_set_progress_property, self.prog_obj, title = 'Calculating percentiles'

  if strlen(stack_name) eq 0 then return
  envi_open_file, stack_name, r_fid = fid
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, data_ignore_value = ignore_value
  
  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]
    
  if ptr_valid(self.percentile_low) then ptr_free, self.percentile_low
  if ptr_valid(self.percentile_high) then ptr_free, self.percentile_high
  
  self.percentile_low = ptr_new(fltarr(ns, nl)
  self.percentile_high = ptr_new(fltarr(ns, nl)
  
  percentile = [self.percentile_low, self.percentile_high]
  pperc = rebin(transpose(percentile), ns, 2)
  pos = indgen(nb)
  ivx = []  ; data locations where ignore_data values have been found
  cnt_val = intarr(ns, nb)
  for l = 0, nl - 1 do begin
    data = envi_get_slice(fid = fid, line = l, xs, 0, xe = ns - 1, pos = pos)
    if hasIgnore then begin
      ivx = where(data eq ignore_value, ivx_cnt)
      if ivx_cnt gt 0 then data[ivx] = !values.f_nan
    endif

    ; calculate percentile indices into the data
    valids = where(finite(data))
    cnt_val[*] = 0
    cnt_val[valids] = 1
    agg_val = total(cnt_val, 2) ; determine valid band values per pixel
    nb_mat = rebin(agg_val, ns, 2)
    py = long(nb_mat * pperc)
    pai = px + py * ns   ; calc the index of the percentile based on actual non-nan values, per pixel
    
    ; sort the data per pixel, NAN values are stored at the end of the data
    ix = sort(data)
    h = histogram(ix mod ns, reverse_indices = ri)  ; order all indices per column
    six = transpose(reform(ix[ri[ns + 1L : *]], nb, ns))  ; indices are now arranged as BIP
    sorted = data[six]  ; data is now ordered as BIP. The stack values are now sorted per pixel
    
    *(self.percentile_low)[*, l] = sorted[pai][*, 0]  ; select the percentile value per pixel for all percentiles
    *(self.percentile_high)[*, l] = sorted[pai][*, 1]
  endfor
  
;  endif else begin
;    dbase = *(self.precip)
;    ; use the timeseries itself to calculate the 95/99 percentile
;    ; make sure the base years are inside the full timeseries
;    bsy = max([self.start_year, self.base_start_year])
;    bey = min([self.end_year, self.base_end_year])
;    nrs_get_dt_indices, [julday(1, 1, bsy), julday(12, 31, bey)], period = 'day', julian_out = jd_base
;    jd_start = julday(1, 1, self.start_year)
;
;    base_ix = jd_base - jd_start ; index of base years inside timeseries
;    base = dbase[base_ix]
;  endelse
;  sel_ix = where(base gt 1, sel_cnt)  ; takes care of missing also (handles missing as NaN or missing lower than 0)
;  prec = base[sel_ix]   ; only wet days
;
;  prec = prec[sort(prec)]
;  nr_prec = n_elements(prec)
;  p95_ix = long(0.95 * nr_prec) ; percentile by nearest rank
;  p99_ix = long(0.99 * nr_prec)
;  self.percentile_95 = prec[p95_ix]
;  self.percentile_99 = prec[p99_ix]

end

pro NrsStackAggregate__define
  compile_opt idl2

  void = { NrsStackAggregate, $
    inherits idl_object $ ; superclass
    , stack_name:       '' $
    , percentile_low:          ptr_new() $; image band containing the per pixel lower percentile; if null needs to be calculated
    , percentile_high:         ptr_new() $; image band containing the per pixel higher percentile; if null needs to be calculated
    , p_low:      0 $   ; low percentile in %
    , p_high:     0 $  ; high percentile in %
;    , base_start_year: 0 $
;    , base_end_year:   0 $
    , prog_obj:        obj_new() $;!null $
  }

end