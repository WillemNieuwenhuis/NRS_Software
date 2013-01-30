;+
; :Description:
;    Calculate Standardized Precipitation Index.
;
; :Params:
;    image_name : in, required
;      Name of the input timeseries. The timesteps are assumed to be monthly
;    time_scales : in, required
;      An array with one or more time scales
;
; :Keywords:
;    outname : in
;      The name of the output result
;    prog_obj : in, optional
;      Progressbar object to indicate progress of the calculation
;    cancelled : in, optional
;      If a progressbar is used, signals that the user stopped the calculation
;
; :Author: nieuwenhuis
;-
pro nrs_precipitation_index, image_name $
                       , time_scales $
                       , outname = outname $
                       , start_year, end_year $
                       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image_name, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  cancelled = 0
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  if n_elements(undef) eq 0 then undef = -99.0
  if undef eq 1.0e-34 then undef = -99.0
  
  if strlen(strtrim(outname, 2)) eq 0 then $
    outname = getOutname(image_name, ext = '.', postfix = '_spi')

  data = make_array(ns, nl, nb, type = dt)
  for b = 0, nb - 1 do begin
    data[*, *, b] = envi_get_data(fid = fid, dims = dims, pos = b)
  endfor
  
  data = reform(data, ns * nl, nb, /overwrite)
  
  nr_months = 12 * (end_year - start_year + 1)
  spidata = make_array(ns * nl, nr_months, n_elements(time_scales), /float) 
  
  do_progress = n_elements(prog_obj) gt 0
  if do_progress eq 1 then begin
    prog_obj->setProperty, title = 'Calculating SPI'
    prog_obj->Start
  endif  
  cancelled = 0
  
  bsz = ns * nl
  for p = 0, bsz - 1 do begin
    if nrs_update_progress(prog_obj, p, bsz, cancelled = cancelled) then return
    
    precip_ar = data[p, *]
    nrs_spi_gamma, precip_ar, time_scales, start_year, end_year, undef, spi_out
    spidata[p, *, *] = spi_out
  end

  spidata = reform(spidata, ns, nl, nr_months, n_elements(time_scales), /overwrite)
  yy = indgen(nr_months) / 12 + start_year
  mm = (indgen(nr_months) mod 12) + 1
  yymm = [transpose(yy), transpose(mm)]
  bnames = string(yymm, format = '("Year.Month ", i4, ".", i02)')
  ts_str = string(time_scales, format = '(i02)')
  for ts = 0, n_elements(time_scales) - 1 do begin
    outfilename = getoutname(outname, postfix = ts_str[ts], ext = '.')
    outdata = spidata[*, *, *, ts]
    envi_write_envi_file, outdata, out_name = outfilename $
            , bnames = bnames $
            , map_info = mi $
            , xstart = xs, ystart = ys $
            , data_ignore_value = undef
  endfor
end

;+
; :Description:
;   Compute the Standardized Precipitation Index
;   using an incomplete gamma distribution function to estimate
;   probabilities.
;
; :Params:
;    precip : in
;      Precipitation time series (monthly values)
;    time_scales : in
;      Array with the time scales for the SPI computation
;    ibegyr : in
;      Start year
;    iendyr : in
;      End year
;    nodata : in
;      The value indicating no valid data
;    spi_ar : out
;      Array for the SPI values
;
; :Author: nieuwenhuis
; 
; :References:
;   <ul>
;   <li>_Numerical Recipes in C_ by Flannery, Teukolsky and Vetterling
;   Cambridge University Press, ISBN 0-521-35465-x
;
;   <li>_Handbook of Mathematical Functions_ by Abramowitz and Stegun
;   Dover, Standard Book Number 486-61272-4
;   <ul>
;
;-
pro nrs_spi_gamma, precip, time_scales, ibegyr, iendyr, nodata, spi_ar
  compile_opt idl2

  freq_per_year = 12
  maxyrs = iendyr - ibegyr + 1
  spi_ar = dblarr(maxyrs * freq_per_year, n_elements(time_scales))
  spi = dblarr(maxyrs * freq_per_year)
  alpha_ar = dblarr(freq_per_year)
  beta_ar = dblarr(freq_per_year)
  pzero_ar = dblarr(freq_per_year)
  for ts = 0, n_elements(time_scales) - 1 do begin
    nr_months = time_scales[ts]
    ; initialize arrays
    spi[*] = nodata
    alpha_ar[*] = nodata
    beta_ar[*] = nodata
    pzero_ar[*] = nodata
    ; calculate the cumulative precipitation for current timescale
    for ei = nr_months - 1, maxyrs * freq_per_year - 1 do begin
      bi = ei - (nr_months - 1)
      six = where(precip[bi : ei] eq nodata, count)
      if count eq 0 then $
        spi[ei] = total(precip[bi : ei])
    endfor
  
    ; collect the cumulative precipitation data for each month in the time series
    ; and fit using incomplete gamma distribution
    for i = 0, freq_per_year - 1 do begin
      index = indgen(maxyrs) + nr_months + i
      data = spi[index]
      ix = where(data ne nodata, count)
      if count gt 0 then begin
        nrs_gamma_fitting, data[ix], alpha, beta, pzero
        im = ((nr_months + i - 1) mod freq_per_year)
        alpha_ar[im] = alpha
        beta_ar[im] = beta
        pzero_ar[im] = pzero
      endif  
    
    endfor
  
    ; Calculate the actual SPI values
    ix = where(spi ne nodata, count)
    for i = 0, count - 1 do begin
      j = ix[i]
      im = j mod freq_per_year
      probability = nrs_gamma_dist(spi[j], alpha_ar[im], beta_ar[im], pzero_ar[im])
      spi[j] = nrs_trans_prob(probability)  ; transform to standard normal distributed value
    endfor
    
    spi_ar[*, ts] = spi
  endfor
end

;+
; :Description:
;   Transform cumulative probability to standard normal distribution; handles arrays as well
;   
; :Params:
;   probs : in
;     cumulative probability
;
; :References:
;   Abromowitz and Stegun, Handbook of Mathematical Functions, p. 933
;
; :Author: nieuwenhuis
;-
function nrs_trans_prob, probs
  compile_opt idl2

  cnt = n_elements(probs)
  res = dblarr(cnt)
  c0 = 2.515517
  c1 = 0.802853
  c2 = 0.010328
  d1 = 1.432788
  d2 = 0.189269
  d3 = 0.001308
  for ii = 0, cnt - 1 do begin
    prob = probs[ii]
    if prob gt 0.5 then begin
      sign = 1.0
      prob = 1.0 - prob
    endif else begin
      sign = -1.0
    endelse
    if prob lt 0.0 then begin
      res[ii] = !VALUES.D_NAN
    endif
    if prob eq 0.0 then begin
      res[ii] = 1.0e37 * sign
    endif
    
    t = sqrt(alog(1.0 / (prob * prob)))
    res[ii] = (sign * (t - ((((c2 * t) + c1) * t) + c0) / $
            ((((((d3 * t) + d2) * t) + d1) * t) + 1.0)))
  endfor
  
  return, res
end

;+
; :Description:
;   Estimate incomplete gamma parameters.
;
; :Params:
;    datarr : in
;      data array
;    alpha : out
;      gamma parameter
;    beta : out
;      gamma parameter
;    pzero : out
;      probability of zero
;
; :Keywords:
;    A : out
;      gamma parameter
; 
; :References:
;   Thom, HCS, A note on gamma distribution, Monthly Weather Review, Vol 68, 1958, pg. 117-122
; 
; :Author: nieuwenhuis
;-
pro nrs_gamma_fitting, datarr, alpha, beta, pzero, A = A
  compile_opt idl2

  n = n_elements(datarr)
  ix = where(datarr gt 0.0, nact)
  
  if nact le 1 then begin
    ; data is all zeroes, or rubbish, return something anyway 
    A = 0.0
    alpha = 0.0
    beta = nact eq 0 ? 1.0 : datarr[ix[0]]
    return
  endif

  pzero = (n - nact) /n
  sum = total(datarr[ix])
  sumlog = total(alog(datarr[ix]))
  
  ; Approximation of Thom, 1958
  av = sum / nact
  A = alog(av) - sumlog / nact
  alpha = (1.0 + sqrt(1.0 + 4.0 * A / 3.0)) / (4.0 * A)
  beta = av / alpha
end

;+
; :Description:
;   Compute probability of gamma function <= x using incomplete gamma parameters.
;
; :Param:
;   x : in
;     value
;   alpha : in
;     gamma parameter
;   beta : in
;     gamma parameter
;   pzero : in
;     probability of zero
;
; Return:
;   Probability gamma <= x.
;   
; :Author: nieuwenhuis
;-
function nrs_gamma_dist, x, alpha, beta, pzero
  compile_opt idl2

  if x le 0.0 then return, pzero
    
  res = pzero + (1.0 - pzero) * igamma(alpha, x / beta, /double)
  
  return, res
end
  
