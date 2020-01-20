function NrsClimatology::init, _extra = ex
  compile_opt idl2

  ; call our superclass initialization method.
  void = self->idl_object::init()
  if (isa(ex)) then self->setproperty, _extra = ex
;  self.percentile_95 = !values.f_nan
;  self.percentile_99 = !values.f_nan

  return, 1
end

;+
; :Description:
;    Calculate a normalized weight window for a period of Ny years and a window size of N12 days
;    (See: ERA-Interim Daily Climatology, Martin Janoušek, ECMWF, january 2011)
;    It is build for all locations in the datacube; If the datacube has not yet been build
;    a single weight vector is created.
;    The created array/vector will have length (2 * n12 + 1) * ny in the time dimension (last dimension)
;    where the pattern repeats per year.
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
pro NrsClimatology::calc_weights
  compile_opt idl2, logical_predicate

  if self.weights_valid eq 1 then return
  
  if ptr_valid(self.weights) then ptr_free, self.weights
  if ptr_valid(self.qweight) then ptr_free, self.qweight
  
  if n_elements(self.N12) eq 0 then self.N12 = 30

  if n_elements(self.Ny) eq 0 then self.Ny = 20
  
  n12 = self.n12
  ny = self.ny
  win_size = 2 * n12 + 1
  factor = 3.0 * (N12 + 1) / (Ny * (2 * N12 + 1) * (2 * N12 + 3))
  single = factor * (1 - ( (findgen(2 * N12 + 1) - N12) / (N12 + 1) ) ^ 2)
  single = reform(rebin(single, win_size, self.ny), win_size * self.ny)

  if ptr_valid(self.datacube) then begin
    sz = size(*self.datacube, /dim)
    xdim = sz[0]
    ydim = sz[1]
    wgt = rebin(transpose(single), xdim * ydim, win_size * self.ny)
    wgt = reform(wgt, xdim, ydim, win_size * self.ny, /overwrite)
  endif else wgt = single

  self.weights = ptr_new(wgt)
  self.qweight = ptr_new(single)
  self.weights_valid = 1
end


;+
; :Description:
;    Determine date indices based on the julian dates. Assumed is that the data
;    contains daily data without missing dates. 
;    The indices include the range of the moving window.
;    The indices will not include the extra day in leap years; so 29 februari is not evaluated
;    
;    The resulting indices point to the actual bands in the data array.
;
; :Author: nieuwenhuis
; :Obsolete:
;-
pro NrsClimatology::calc_day_indices
  compile_opt idl2, logical_predicate

  years = indgen(self.end_year - self.start_year + 1) + self.start_year
  nrs_get_dt_indices, [julday(1, 1, bsy) - self.n12, julday(12, 31, bey)] + self.n12, period = 'day', julian_out = jd_base
  feb29 = julday(2,29,years)
  caldat, feb29, mm, dd, yy
  ix = where(mm eq 2)
  ix = feb29[ix] - jd_base[0]  ; get the indices of all 29 feb days (leap years)
  jd_base[ix] = 0
  ix = where(jd_base ne 0)
  jds = jd_base[ix] - jd_base[0]   ; now we have removed all 29 feb from the indices( the data still contains them)

  if ptr_valid(self.index) then ptr_free, self.index
  self.index = ptr_new(jds)

end

function nrs_build_regex_from_mask, mask
  compile_opt idl2, logical_predicate

  ; mask f.e.: '.yyyy.mm.dd' becomes: '[^0-9]*([0-9]{4})[^0-9]([0-9]{2})[^0-9]([0-9]{2})'
  mask += '.' ; add aditional char to make sure to include the last group
  regex = ''
  yp = -1
  mp = -1
  dp = -1
  prev_char = ' '
  prev_pos = 0
  cnt = 0
  for i = 0, strlen(mask) - 1 do begin
    c = mask.charat(i)
    if c eq prev_char then begin
      cnt++
    endif $
    else begin
      case prev_char of
        ' ' : break
        '.' : begin
                regex += '[^0-9]*'
                break 
              end
        'y' : begin
                if yp eq -1 then yp = i
                regex += '([0-9]{' + string(i - prev_pos, format = '(i0)') + '})'
              end
        'm' : begin
                if mp eq -1 then mp = i
                regex += '([0-9]{' + string(i - prev_pos, format = '(i0)') + '})'
              end
        'd' : begin
                if dp eq -1 then dp = i
                regex += '([0-9]{' + string(i - prev_pos, format = '(i0)') + '})'
                break
              end
      endcase
      prev_char = c
      prev_pos = i
    endelse
  endfor
  raw = [yp, mp, dp]
  rix = sort(raw)

  return, {date_order, mask:regex, order: rix}   
end


pro NrsClimatology::load_data
  compile_opt idl2, logical_predicate

  if strlen(self.base_folder) eq 0 then return
  
  res = nrs_build_regex_from_mask(self.date_mask)

  ; recursively get all data filenames in the basefolcubeder
  ; and extract the date for each file from the filename
  filelist = file_search(self.base_folder, self.file_mask)  ; get all files (IDL sorts the files by name)
  jd = make_array(n_elements(filelist), /long, /nozero) 
  for f = 0, n_elements(filelist) - 1 do begin
    parts = fix( (stregex(file_basename(filelist[f]), res.mask, /extract, /subex))[1:-1])
    jd[f] = julday(parts[res.order[1]], parts[res.order[2]], parts[res.order[0]])
  endfor
  
  ; check and select only those file needed for the user selection
  sy = self.start_year
  ey = self.end_year
  jdstart = julday(1, 1, sy) - self.n12
  jdend = julday(12, 31, ey) + self.n12
  
  sp = where(jd eq jdstart, cnts)
  ep = where(jd eq jdend, cnte)
  if cnts * cnte ne 1 then begin
    void = error_message('Not enough data for the date range specified', title = 'Climatology', /info)
    return
  endif

  jd = jd[sp : ep]
  files = filelist[sp : ep]
  
  ; also remove the leap day data
  caldat, jd, mm, dd, yy
  t1 = (mm eq 2) * (dd eq 29)
  ix = where(t1 eq 0)
  jd = jd[ix]   ; julian dates except all 29 feb

  if ptr_valid(self.index) then ptr_free, self.index
  self.index = ptr_new(jd)

  ; the final list of data files required
  files = files[ix]  ; exclude 29 feb from analysis
  caldat, jd, mm, dd, yy
  
  ; Now start filling the cube
  ; first setup the datacube
  e = envi(/headless)
;  e.preferences['data manager:launch after open'] = 'Never'   ; don't display data manager
  ras = e.OpenRaster(files[0])
  if (self.xnum * self.ynum) eq 0 then begin
    self.xstart = 0
    self.ystart = 0
    self.xnum = ras.ncolumns
    self.ynum = ras.nrows
  endif
  data = make_array(self.xnum, self.ynum, n_elements(files), /float, /nozero)
  ras.close

  ; at the data from all selected files (note: assuming one band per raster)
  nrs_set_progress_property, self.prog_obj, title = 'Loading data', /start  
  for f = 0, n_elements(files) - 1 do begin
    if f mod 10 eq 0 then begin
      void = nrs_update_progress(prog_obj, f, n_elements(files))
    endif
    ras = e.OpenRaster(files[f])
    data[*, *, f] = ras.GetData(bands = [0], sub_rect = [self.xstart, self.ystart $
                                           , self.xstart + self.xnum - 1, self.ystart + self.ynum - 1])
    ras.close
  endfor
  
  if ptr_valid(self.datacube) then begin
    ptr_free, self.datacube
  endif
  self.datacube = ptr_new(data)
  
end

;+
; :Description:
;    Calculate the weighted statistics (mean, variance) on the datacube
;    Note that the weights have been precalculated to directly give the mean and variance
;    
;    Given the definition the mean is calculated with an extra large window size 2 * N12 to
;    accomodate the calculation of the variance that needs the additional mean values.
;    Thus needed are data in the specified period (1-1-<start year> to 31-12-<end year>) 
;    extended with 30 days on both sides (an additional 60 days) 
;
; :Author: nieuwenhuis
; :History:
;   - 8 November 2019: created
;-
pro NrsClimatology::statistics
  compile_opt idl2, logical_predicate

  if self.weights_valid eq 0 then self.calc_weights
  
  wgt = *(self.weights)     ; get the weights for the window  

  nrdays = 365

  dims = size(*self.datacube, /dim)    ; the size of the cube with the additional required day data

  clim_mean = fltarr([dims[0], dims[1], nrdays])
  clim_var  = fltarr([dims[0], dims[1], nrdays])
  
  ; first calculate the climatic mean; needed also for the climatic anomaly.
  ; indices are zero-based
  
  nrs_set_progress_property, self.prog_obj, title = 'Calculate mean', /start
;  e = envi(/headless)
;  Channel = e.GetBroadcastChannel()
;  Abort = ENVIAbortable()
;  Start = ENVIStartMessage('Climatology', Abort)
;  Channel.Broadcast, Start
;  Progress = ENVIProgressMessage('Calculate mean', 0, Abort)
  
  win_size = 2 * self.n12 + 1
  ix1 = lindgen(self.ny * win_size) mod win_size
  ix2 = (lindgen(self.ny * win_size) / win_size ) * nrdays
  ix = ix1 + ix2    ; index into datacube for data needed for one DOY
  for day = 0, nrdays - 1 do begin    ; handle every DOY
    void = nrs_update_progress(prog_obj, day, nrdays)
;    Progress.Percent = day / nrdays
;    Channel.Broadcast, Progress
    clim_mean[*, *, day] = total((*self.datacube)[*, *, ix] * wgt, 3)
    ix += 1   ; index for next DOY
  endfor
  
  if ptr_valid(self.stat_mean) then ptr_free, self.stat_mean
  self.stat_mean = ptr_new(clim_mean)

  ; now calculate the variance using the mean values calculated above
  ; Note that for the calculations the mean values are accessed modular (at the beginning and end of the year)
  nrs_set_progress_property, self.prog_obj, title = 'Calculate variance', /start
;  Progress = ENVIProgressMessage('Calculate variance', 0, Abort)

  ix = ix1 + ix2    ; reset index into datacube for data needed for one DOY
  ixm = (lindgen(win_size) - self.n12 + nrdays) mod nrdays
  ixm = reform(rebin(ixm, win_size, self.ny), self.ny * win_size, /overwrite)     ; index into clim_mean (day 0 at 0); wrap around if needed
  for day = 0, nrdays - 1 do begin    ; handle every DOY
    void = nrs_update_progress(prog_obj, day, nrdays)
;    Progress.Percent = day / nrdays
;    Channel.Broadcast, Progress
    
    clim_var[*, *, day] = total( ((*self.datacube)[*, *, ix] - clim_mean[*, *, ixm])^2 * wgt, 3)
    ix += 1           ; to next DOY in datacube
    ixm = (ixm + 1) mod nrdays  ; to next DOY in mean cube
  endfor

  if ptr_valid(self.stat_var) then ptr_free, self.stat_var
  self.stat_var = ptr_new(clim_var)

;  Finish = ENVIFinishMessage(Abort)
;  Channel.Broadcast, Finish
  
end

;+
; :Description:
;    Calculate the weighted quantiles on the datacube. One or more quantiles
;    can be calculated at the same time. Quantile values range from [0 .. 1]
;    Note that the weights have been precalculated to directly give the mean and variance
;
;    For each DOY each quantile is calculated with an large temporal window size (2 * N12 days) around
;
; :Params:
;    quantiles, in, required:
;       Specify one or more quantiles to be calculated
;
; :Author: nieuwenhuis
; :History:
;   - 8 November 2019: created
;-
pro NrsClimatology::quantiles, quantiles
  compile_opt idl2, logical_predicate

  if n_elements(quantiles) eq 0 then begin
    void = error_message('No quantiles specified; stopping')
    return
  endif

  if ptr_valid(self.quantiles) then ptr_free, self.quantiles
  quantiles = quantiles[sort(quantiles)]
  self.quantiles = ptr_new(quantiles)

  if self.weights_valid eq 0 then self.calc_weights

  win_size = 2 * self.n12 + 1
  wgt_single = *(self.qweight)     ; get the 1D-weight vector with length of windows size (2 * n12 + 1) * ny

  nrdays = 365

  dims = size(*self.datacube, /dim)    ; the size of the cube with the additional required day data

  ; Calculate quantiles
  quant = make_array([dims[0:1], nrdays, n_elements(quantiles)], /nozero)   ; make space for all quantiles
  clim_min = fltarr([dims[0], dims[1], nrdays])
  clim_max = fltarr([dims[0], dims[1], nrdays])

  ; collect the samples
  nrs_set_progress_property, self.prog_obj, title = 'Calculate quantiles', /start
  ix1 = lindgen(self.ny * win_size) mod win_size
  ix2 = (lindgen(self.ny * win_size) / win_size ) * nrdays
  ix = ix1 + ix2    ; index into datacube for data needed for one DOY
  for day = 0, nrdays - 1 do begin    ; handle every DOY
    void = nrs_update_progress(prog_obj, day, nrdays)

    for r = 0, dims[1] - 1 do begin
      for c = 0, dims[0] - 1 do begin
        vals = (*self.datacube)[c, r, ix]
        qx = sort(vals)  ; sort all samples
        qs = vals[qx]    ; samples are now sorted
        wx = wgt_single[qx]     ; keep weights aligned with samples
        wsum = total(wx, /cum)        ; cumulative weights; (last element should be 1.0)
        vl = value_locate(wsum, quantiles)   ; find the quantiles positions in the cumulative weights (lower weighted)
        quant[c, r, day, *] = qs[vl]         ; get the samples for the quantile positions
        clim_min[c, r, day] = qs[0]
        clim_max[c, r, day] = qs[-1]
      endfor
    endfor
    
    ix += 1     ; index to next DOY
  endfor
  
  if ptr_valid(self.stat_quant) then ptr_free, self.stat_quant
  self.stat_quant = ptr_new(quant)

  if ptr_valid(self.stat_min) then ptr_free, self.stat_min
  self.stat_min = ptr_new(clim_min)

  if ptr_valid(self.stat_max) then ptr_free, self.stat_max
  self.stat_max = ptr_new(clim_max)

end

pro NrsClimatology::cleanup
  compile_opt idl2

  if ptr_valid(self.datacube) then ptr_free, self.datacube
  if obj_valid(self.prog_obj) then self.prog_obj->destroy
  if ptr_valid(self.index) then ptr_free, self.index
  if ptr_valid(self.weights) then ptr_free, self.weights
  if ptr_valid(self.qweight) then ptr_free, self.qweight
  if ptr_valid(self.stat_mean) then ptr_free, self.stat_mean
  if ptr_valid(self.stat_var) then ptr_free, self.stat_var
  if ptr_valid(self.stat_quant) then ptr_free, self.stat_quant
  if ptr_valid(self.quantiles) then ptr_free, self.quantiles

  ; call our superclass cleanup method
  self->idl_object::cleanup
end

pro NrsClimatology::setproperty, start_year = start_year, end_year = end_year $
                               , base_folder = base_folder, file_mask = file_mask $
                               , date_mask = date_mask $
                               , xstart = xstart, xnum = xnum, ystart = ystart, ynum = ynum $
                               , datacube = datacube, n12 = n12, ny = ny $
                               , prog_obj = prog_obj
  compile_opt idl2, logical_predicate

  ; if user passed in a property, then set it.
  if (isa(start_year))  then self.start_year = start_year
  if (isa(end_year))    then self.end_year = end_year
  if (isa(base_folder)) then self.base_folder = base_folder
  if (isa(file_mask))   then self.file_mask = file_mask
  if (isa(date_mask))   then self.date_mask = date_mask
  if (isa(xstart))      then self.xstart = xstart     
  if (isa(xnum))        then self.xnum = xnum
  if (isa(ystart))      then self.ystart = ystart
  if (isa(ynum))        then self.ynum = ynum
  if (isa(n12))         then begin
    self.n12 = n12
    self.weights_valid = 0
  endif
  if (isa(ny))          then begin
    self.ny = ny
    self.weights_valid = 0
  endif
  if (isa(datacube))    then begin
    if ptr_valid(self.datacube) then ptr_free, self.datacube
    self.datacube = ptr_new(datacube)
  endif

  if isa(prog_obj, "PROGRESSBAR") then begin
    if obj_valid(self.prog_obj) then self.prog_obj->destroy
    self.prog_obj = prog_obj
  endif
  
  ; check #years
  if self.start_year lt self.end_year then begin
    ny = self.end_year - self.start_year + 1
    if (self.ny eq 0 ) || (ny ne self.ny) then begin
      self.ny = ny
      self.weights_valid = 0
    endif
  endif
  

end

pro NrsClimatology::getproperty, start_year = start_year, end_year = end_year, ny = ny $
                               , base_folder = base_folder, file_mask = file_mask $
                               , datacube = datacube, weights = weights, qweight = qweight, dims = dims $
                               , mean_data = mean_data, var_data = var_data $
                               , min_data = min_data, max_data = max_data $
                               , julian = julian $
                               , quant_data = quant_data $
                               , quantiles = quantiles
  compile_opt idl2, logical_predicate

  if (isa(self)) then begin
    ; user asked for an "instance" property.
    if (arg_present(start_year))  then start_year = self.start_year
    if (arg_present(end_year))    then end_year = self.end_year
    if (arg_present(base_folder)) then base_folder = self.basefolder
    if (arg_present(file_mask))   then file_mask = self.file_mask
    if (arg_present(date_mask))   then date_mask = self.date_mask
    if (arg_present(n12))         then n12 = self.n12
    if (arg_present(ny))          then ny = self.ny
    if (arg_present(julian))      then julian = *(self.index)
    if (arg_present(quantiles))   then quantiles = *(self.quantiles)
    if (arg_present(datacube))    then begin
      if ptr_valid(self.datacube) then datacube = *(self.datacube) else datacube = []
    endif
    if (arg_present(dims))        then begin
        if ptr_valid(self.datacube) then dims = size(*(self.datacube), /dim) $
        else dims = []
    endif
    if (arg_present(mean_data))    then begin
      if ptr_valid(self.stat_mean) then mean_data = *(self.stat_mean) else mean_data = []
    endif
    if (arg_present(var_data))    then begin
      if ptr_valid(self.stat_var) then var_data = *(self.stat_var) else var_data = []
    endif
    if (arg_present(quant_data))  then begin
      if ptr_valid(self.stat_quant) then quant_data = *(self.stat_quant) else quant_data = []
    endif
    if (arg_present(min_data))  then begin
      if ptr_valid(self.stat_min) then min_data = *(self.stat_min) else min_data = []
    endif
    if (arg_present(max_data))  then begin
      if ptr_valid(self.stat_max) then max_data = *(self.stat_max) else max_data = []
    endif
    if (arg_present(weights))    then begin
      if self.weights_valid eq 0 then self.calc_weights
      if ptr_valid(self.weights) then weights = *(self.weights) else weights = []
    endif
    if (arg_present(qweight))    then begin
      if self.weights_valid eq 0 then self.calc_weights
      if ptr_valid(self.qweight) then qweight = *(self.qweight) else qweight = []
    endif
  endif

end

pro NrsClimatology__define
  compile_opt idl2

  ; datacube contains the entire 3D-cube. This includes the images needed for the window (so n12 extra
  ; layers in the beginning and as the end layers of the cube). Assumed is that the relevant data starts
  ; at 1 jan and ends at 31 dec.
  void = { NrsClimatology, $
    inherits idl_object $ ; superclass
    , base_folder:     '' $               ; name of the base data folder
    , file_mask:       '' $               ; pattern to collect all images
    , xstart:          0L $               ; start X-pixel location 
    , xnum:            0L $               ; num of X-pixels to read
    , ystart:          0L $               ; start Y-pixel location
    , ynum:            0L $               ; num of pY-ixels to read
    , date_mask:       '' $               ; mask to match filename to date; f.e. "...yyyy.mm.dd" on "2t_1979_12_31.tif" 
    , need_loading:    0L $               ;
    , datacube:        ptr_new() $        ; BSQ; data cube includes addtional data required for the moving window
    , stat_mean:       ptr_new() $        ; BSQ datacube, contains climatic mean (weighted average result)
    , stat_var:        ptr_new() $        ; BSQ datacube; contains the anomaly around the climatic mean result.
    , stat_quant:      ptr_new() $        ; BSQ datacube; contains the weighted quantile values (spatial * nrdays * nr_quantiles)
    , stat_min:        ptr_new() $        ; BSQ datacube; contains minimum data value
    , stat_max:        ptr_new() $        ; BSQ datacube; contains maximum data value
    , start_year:      0 $                ; start year of data cube
    , end_year:        0 $                ; end year of data cube (contains at least 30 days in the last year)
    , n12:            30 $                ; N12: size of the moving window
    , ny:             20 $                ; NY: number of years in the timeseries
    , quantiles:       ptr_new() $        ; list with quantiles (values between 0 and 1) to extract  
    , weights_valid:   0 $                ; if zero, weights need to be recalculated
    , weights:         ptr_new() $        ; the weights for the weighted moving window (spatial as well: 3D vector)
    , qweight:         ptr_new() $        ; weights for all samples in the entire period (1D vector)
    , index:           ptr_new() $        ; the date index into the daily data cube
    , prog_obj:        obj_new() $;!null $
  }

end

