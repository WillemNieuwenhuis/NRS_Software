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

  if ptr_valid(self.percentile_low) then ptr_free, self.percentile_low
  if ptr_valid(self.percentile_high) then ptr_free, self.percentile_high
;  if ptr_valid(self.base) then ptr_free, self.base
;  if obj_valid(self.prog_obj) then self.prog_obj->destroy

  ; call our superclass cleanup method
  self->idl_object::cleanup
end

pro NrsStackAggregate::getproperty $
          , base_start_date = base_start_date, base_end_date = base_end_date
  compile_opt idl2

  if (isa(self)) then begin
    ; user asked for an "instance" property.
    if (arg_present(start_year)) then start_year = self.start_year
    if (arg_present(end_year)) then end_year = self.end_year
    if (arg_present(base_start_date)) then base_start_date = self.base_start_date
    if (arg_present(base_end_date)) then base_end_date = self.base_end_date
  endif
end

pro NrsStackAggregate::check_set_perc, low, high
  compile_opt idl2
  
  self.need_recalc = 0
  if low gt high then begin
    dummy = low
    low = high
    high = dummy
  endif
  if self.p_low ne low then begin
    self.p_low = low
    self.need_recalc = 1
  endif
  if self.p_high ne high then begin
    self.p_high = high
    self.need_recalc = 1
  endif
end

pro NrsStackAggregate::setproperty, perc_low = perc_low, perc_high = perc_high, use_percentiles = use_percentiles $
  , base_start_date = base_start_date, base_end_date = base_end_date $
  , period = period $
  , stack_name = stack_name $
  , outlier_name = outlier_name $
  , prog_obj = prog_obj
  compile_opt idl2

  ; if user passed in a property, then set it.
  low = isa(perc_low) ? perc_low : self.p_low
  high = isa(perc_high) ? perc_high : self.p_high
  self.check_set_perc, low, high
  if isa(use_percentiles) then self.use_percentiles = use_percentiles
  
  if isa(base_start_date) then self.base_start_date = base_start_date
  if isa(base_end_date) then self.base_end_date = base_end_date
  if isa(period) then self.period = period
  if isa(stack_name) then begin
    fi = file_info(stack_name)
    if fi.exists && ~fi.directory then $
      self.stack_name = stack_name $
    else self.stack_name = '' 
  endif
  if isa(outlier_name) then begin
     if strlen(outlier_name) gt 0 then begin
       self.outname_outlier = outlier_name
       self.generate_outlier_index = 1
     endif else self.generate_outlier_index = 0
  endif
  if isa(prog_obj, "PROGRESSBAR") then begin
    if obj_valid(self.prog_obj) then self.prog_obj->destroy
    self.prog_obj = prog_obj
  endif

end

pro NrsStackAggregate::calculate_percentiles
  compile_opt idl2, logical_predicate

  if strlen(self.stack_name) eq 0 then return
  
  ; open the stack to calculate the percentiles
  envi_open_file, self.stack_name, r_fid = fid
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, data_ignore_value = ignore_value, data_type = dt
  
  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]
    
  nrs_set_progress_property, self.prog_obj, title = 'Calculating percentiles'

  if ptr_valid(self.percentile_low) then ptr_free, self.percentile_low
  if ptr_valid(self.percentile_high) then ptr_free, self.percentile_high
  
  self.percentile_low = ptr_new(fltarr(ns, nl))
  self.percentile_high = ptr_new(fltarr(ns, nl))
  
  percentile = [self.p_low, self.p_high] / 100.0  ; percentage to value
  perc_count = n_elements(percentile)
  pperc = rebin(transpose(percentile), ns, 2)
  px = rebin(indgen(ns), ns, perc_count)
  pos = indgen(nb)
  ivx = []  ; data locations where ignore_data values have been found
  cnt_val = intarr(ns, nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(self.prog_obj, l, nl, cancelled = cancelled) then begin
      break
    endif

    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos)
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
    
    (*(self.percentile_low))[*, l] = (sorted[pai])[*, 0]  ; select the percentile value per pixel for the low percentile
    (*(self.percentile_high))[*, l] = (sorted[pai])[*, 1]  ; select the percentile value per pixel for the high percentile
  endfor
;  envi_file_mng, id = fid, /remove  ; close the image stack
  if ~cancelled then self.need_recalc = 0
  
  nrs_set_progress_property, self.prog_obj, title = 'Percentiles are calculated'
end

;+
; :Description:
;    Aggregate the stack, named in stack_name, using method as aggregation function
;    If stack_name or method is not specified, return empty result
;
;    The aggregation optionally takes into account upper an lower limits, depending on the use_percentiles field
;    The limits are calculated based on the p_low and p_high fields. Use setproperty to set these values
;
; :Keywords:
;    method: in, required
;
; :Author: nieuwenhuis
;-
function NrsStackAggregate::aggregate, method = method
  compile_opt idl2, logical_predicate

  if strlen(self.stack_name) eq 0 then return, []

  methods = ['sum', 'mean', 'min', 'max', 'stdev', 'median']
  method_ix = where(strlowcase(method) eq methods, cnt)
  if cnt eq 0 then return, []
  
  ; open the stack to calculate the percentiles
  envi_open_file, self.stack_name, r_fid = fid
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, data_ignore_value = ignore_value, data_type = dt
  mi = envi_get_map_info(fid = fid)

  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]

  ; initialize the progress indicator
  nrs_set_progress_property, self.prog_obj, /start, title = 'Aggregating using: ' + method

  ; make sure to calculate the correct percentiles first
  if self.use_percentiles then if self.need_recalc then self.calculate_percentiles
  
  nrs_set_progress_property, self.prog_obj, title = 'Aggregating using: ' + method, /start  ; restart

  out = make_array(ns, nl, type = dt)
  if self.generate_outlier_index then begin
    openw, unit, self.outname_outlier, /get_lun
    
    outliers = bytarr(ns, nb)
    
    sd = nrs_str2julian(self.base_start_date)
    ed = nrs_str2julian(self.base_end_date)
    self.period = nrs_get_period_from_range(sd, ed, nb)
     
    nrs_get_dt_indices, [sd, ed], interval = self.period, julian_out = jul, num_period = per_py
    
    doys = nrs_doy_from_julian(jul, year = doy_year)
    
    bnames = 'Year:Doy=' + string([transpose(doy_year), transpose(doys)], format = '(i04,":",i03)')
  endif
  
  for l = 0, nl - 1 do begin
    if nrs_update_progress(self.prog_obj, l, nl, cancelled = cancelled) then begin
      out = []
      break
    endif
    
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos)
    ; Handle no data value: turn into NaN to exclude them from the aggregation
    if hasIgnore then begin
      ivx = where(data eq ignore_value, ivx_cnt)
      if ivx_cnt gt 0 then data[ivx] = !values.f_nan
    endif
  
    ; Use percentiles: mark outliers as NaN to exclude them from the aggregation
    if self.use_percentiles then begin
      low = rebin((*(self.percentile_low))[*, l], ns, nb)
      high = rebin((*(self.percentile_high))[*, l], ns, nb)
      lix = where((data - low) lt 0, lix_cnt)
      if lix_cnt gt 0 then data[lix] = !values.f_nan
      hix = where((data - high) gt 0, hix_cnt)
      if hix_cnt gt 0 then data[hix] = !values.f_nan
      
      ; calculate outlier day indices if requested
      if self.generate_outlier_index then begin
        outliers[*] = 0
        outliers[lix] = 1 ; doys[lix / ns]  ; report the doy of the outliers
        outliers[hix] = 1 ; doys[hix / ns]
        writeu, unit, outliers
      endif
    endif
    
    case method_ix of
      0 : out[*, l] = total(data, 2, /nan)
      1 : out[*, l] = mean(data, dim = 2, /nan)
      2 : out[*, l] = min(data, dim = 2, /nan)
      3 : out[*, l] = max(data, dim = 2, /nan)
      4 : out[*, l] = stddev(data, dim = 2, /nan)
      5 : out[*, l] = median(data, dim = 2)   ; ignores NAN
    endcase
  endfor
  uix = where(finite(out, /nan), uix_cnt)
  if uix_cnt gt 0 then out[uix] = ignore_value
  
  if self.generate_outlier_index then begin
    close, unit
    free_lun, unit
    
    envi_setup_head, fname = self.outname_outlier $
      , /write $
      , data_type = 1 $ ; byte
      , nb = nb $
      , nl = nl, ns = ns $
      , bnames = bnames $
      , map_info = mi $
      , interleave = 1  ; BIL
    
  endif
  envi_file_mng, id = fid, /remove  ; close the image stack
    
  return, out
end


pro NrsStackAggregate__define
  compile_opt idl2

  void = { NrsStackAggregate, $
    inherits idl_object $ ; superclass
    , stack_name:       '' $
    , percentile_low:          ptr_new() $; image band containing the per pixel lower percentile
    , percentile_high:         ptr_new() $; image band containing the per pixel higher percentile
    , p_low:        0 $     ; low percentile in %
    , p_high:       0 $     ; high percentile in %
    , use_percentiles: 0 $  ; Use set percentiles to calculate the aggregations
    , generate_outlier_index: 0 $
    , need_recalc:     0 $  ; Flag to indicate if the percentiles have changed, and need to be recalculated
    , outname_aggr:     '' $  ; If specified, save to file with this name
    , outname_outlier:  '' $  ; if specified, save the outliers into a file with this name
    , base_start_date: '' $
    , base_end_date:   '' $
    , period:         -1 $    ; product repetition period in days 
    , prog_obj:        obj_new() $;!null $
  }

end