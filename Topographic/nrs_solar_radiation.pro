;+
; :Description:
;   Calculation of (short wave) solar radiation
;   The resulting units for radiation are kJ/m^2/timeperiod
;   To convert to MJ/m^2/day, divide by 1000 * no. of days.
;
; :Params:
;    dem : in, required
;      The terrain elevation
;    start_day : in, required
;      Start day (day of year)
;    end_day : in, required
;      End day (day of year)
;    interval : in, required
;      Time interval in minutes
;
; :Keywords:
;   output_name : in, optional
;     Specify the output name. If not specified the output name will be the
;     input name + '_rad' without extension
;     
; :References:
;   Kumar, L., A. K. Skidmore, et al. (1997). "Modelling topographic variation in solar radiation in a GIS environment." International Journal of Geographical Information Science 11(5): 475-497.
;
; :Author: nieuwenhuis
; :History:
;   Change history::
;     july 2013: WN, improved performance, by calculating hillshade directly
;     feb 2013: WN, created
;-
pro nrs_solar_radiation, dem, start_day, end_day, interval, output_name = energy
  compile_opt idl2, logical_predicate
  
  pi = !dpi
  deg2rad = pi / 180
  time = interval * 2 * pi / (24 * 60) ; convert minutes (time) into radians

  envi_file_query, dem, dims = dims, ns = ns, nl = nl, fname = demname
  mi = envi_get_map_info(fid = dem, undefined = no_csy)
  if mi.proj.type ne 1 then begin
    void = error_message('No Geographic coordinate system', /error)
    return
  endif
  
  envi_convert_file_coordinates, dem, 0.0, 0.0, x_left, y_top, /to_map
  envi_convert_file_coordinates, dem, ns, nl, x_right, y_bot, /to_map
  lat = (y_top + y_bot) / 2
  lon = (x_left + x_right) / 2
  ps_m = nrs_UTM_pixelsize_from_LL(lat, lon, mi.ps)
  
  lat_ext = y_bot - y_top
  nr_strips = ceil(abs(lat_ext))
  dim_ystep = 1 + nl / nr_strips
  
  logfile = getOutname(demname, postfix = '', ext = '.log')
  t1 = systime(/seconds)
  nrs_log_line, logfile, 'Calculation slope/aspect', /append
  intermediate = nrs_solar_prepare_slap_maps(demname, /swap_aspect)
  if n_elements(intermediate) ne 4 then begin
    void = error_message('Failed to calculate slope / aspect image', /error)
    return
  endif
  nrs_log_line, logfile, 'Slope/aspect ready in: ' + nrs_sec_to_string(systime(/seconds) - t1, /time, /hours24), /append
  
  envi_open_file, intermediate[0], r_fid = fid_coslope, /no_realize, /no_interactive_query
  envi_open_file, intermediate[1], r_fid = fid_sincos, /no_realize, /no_interactive_query
  envi_open_file, intermediate[2], r_fid = fid_sinsin, /no_realize, /no_interactive_query
  envi_open_file, intermediate[3], r_fid = fid_slope, /no_realize, /no_interactive_query
  
  envi_file_query, fid_coslope, data_ignore_value = undef
  if undef eq 1.0e-34 then void = temporary(undef)
  doMask = (n_elements(undef) gt 0)
  
  if n_elements(energy) eq 0 then begin
    outname = getOutname(demname, postfix = '_rad', ext = '.dat')
  endif else begin
    outname = energy
  endelse
  t2 = systime(/seconds)
  nrs_log_line, logfile, 'Starting solar radiation calculation for ' + demname, /append

  shade_out = getoutname(demname, postfix = '_shade_temp', ext = '.dat')
  openw, energy_unit, outname, /get_lun
  
  fill_value = doMask ? undef : -9999.0
  initialgrid = fltarr(ns, dim_ystep)
  mask_ix = []
  cnt_valid = 0
  for strip = 0, nr_strips - 1 do begin
    s_y = strip * dim_ystep
    e_y = min([s_y + dim_ystep - 1, nl - 1])
    if (e_y - s_y + 1) ne dim_ystep then begin
      initialgrid = fltarr(ns, e_y - s_y + 1, /nozero) ; special case: last part can be smaller
    endif
    initialgrid[*] = 0.0
    dims_ut = [-1, 0, ns - 1, s_y, e_y]
    nl_cur = e_y - s_y + 1
    nl_center = (e_y + s_y) / 2

    envi_convert_file_coordinates, dem, ns / 2, nl_center, londeg, latdeg, /to_map
    latitude = latdeg * deg2rad

    cosslope = envi_get_data(fid = fid_coslope, dims = dims_ut, pos = [0])
    sincos = envi_get_data(fid = fid_sincos, dims = dims_ut, pos = [0])
    sinsin = envi_get_data(fid = fid_sinsin, dims = dims_ut, pos = [0])
    slope =  envi_get_data(fid = fid_slope, dims = dims_ut, pos = [0]) * deg2rad
    aspect =  envi_get_data(fid = fid_slope, dims = dims_ut, pos = [1]) * deg2rad
    
    nr_elem = ns * nl_cur
    if doMask then begin
      mask_ix = where(cosslope ne undef, cnt_valid)
      if cnt_valid gt 0 then begin
        nr_elem = cnt_valid
        cosslope = temporary(cosslope[mask_ix])
        sincos = temporary(sincos[mask_ix])
        sinsin = temporary(sinsin[mask_ix])
        slope = temporary(slope[mask_ix])
        aspect = temporary(aspect[mask_ix])
      endif
    endif

    daynumber = start_day
    while daynumber le end_day do begin
      di = daynumber - start_day
      dn = end_day - start_day + 1 
      msg = systime() + string(strip+1, nr_strips, di+1, dn, format = '("; Strip=",i0," of ",i0,"; day=",i0," of ",i0)')
print, msg
      nrs_log_line, logfile, msg, /append
      io = 1.367 * (1 + 0.034 * cos(2 * pi * daynumber / 365))  ; eq 7
      decl = 23.45 * deg2rad * sin(2 * pi * (284 + daynumber) / 365) ; eq 4
      
      ; eq 6
      ; (adjusted for latitudes exceeding 66.5° N/S)
      tl_td = -1 * tan(latitude) * tan(decl)
      sign = tl_td ge 0 ? 1 : -1
      if tl_td lt -1 then sunrise = pi $
      else if tl_td gt 1 then sunrise = 0.0 $
      else sunrise = acos(tl_td)      ; regular cases

      sunset = -sunrise
    
      ; ensure calculations are at half the time interval
      hourangle = sunrise - (time / 2)
      
      ; extract individual terms of the equation to speed up calculations
      ;        cosi = cosi1 + cos(decl) * cos(hourangle) * cosi2 + cosi3 * sin(hourangle)
      ; terms of the cosi equation which do not depend on the inner loop
      cosi1 = sin(decl) * (sin(latitude) * cosslope - cos(latitude) * sincos)
      cosi2 = cos(latitude) * cosslope + sin(latitude) * sincos
      cosi3 = cos(decl) * sinsin

      while hourangle ge sunset do begin
         ; eq 2:
         sol_altitude = asin(sin(latitude) * sin(decl) + cos(latitude) * cos(decl) * cos(hourangle))
         test = tan(decl) / tan(latitude)
  
         ; eq 3:
         if (cos(hourangle) gt test) then begin
           sol_zenith = asin(cos(decl) * sin(hourangle) / cos(sol_altitude))
         endif else if (cos(hourangle) lt test) then begin
           sol_zenith = pi - asin(cos(decl) * sin(hourangle) / cos(sol_altitude))
         endif else if (test eq cos(hourangle)) and (hourangle ge 0) then begin
           sol_zenith = pi / 2
         endif else if (test eq cos(hourangle)) and (hourangle lt 0) then begin
           sol_zenith = -pi / 2
         endif

         ; correct for northern lat.s
         azi = (540 - (sol_zenith / deg2rad)) mod 360

         ; eq 11:
         m = sqrt(1229 + (614 * sin(sol_altitude)) ^ 2) - 614 * sin(sol_altitude)
         
         ; eq 15:
         iso = io * 0.56 * (exp(-0.65 * m) + exp(-0.095 * m))
  
         sungrid = cos(sol_altitude) * cosslope + sin(sol_altitude) * sin(slope) * cos(azi - aspect)
  
         cosi = cosi1 + cos(decl) * cos(hourangle) * cosi2 + cosi3 * sin(hourangle)
         shaded = bytarr(nr_elem)
         ix = where(cosi gt 0, count)
         if count gt 0 then shaded[ix] = 1

         if doMask && (cnt_valid gt 0) then begin
           initialgrid[mask_ix] += iso * cosi * sungrid * 60 * interval * shaded
         endif else begin
           initialgrid[*] += iso * cosi * sungrid * 60 * interval * shaded
         endelse

         hourangle = hourangle - time
      endwhile
      daynumber++
    endwhile
    
    ; mask negative values (typically on boundary with undefined values)
    mask_ix = where(initialgrid lt 0, cnt_neg)
    if cnt_neg gt 0 then begin
      initialgrid[mask_ix] = fill_value
    endif
    
    ; mask undefined values
    mask_ix = where(cosslope eq undef, cnt_valid)
    if cnt_valid gt 0 then begin
      initialgrid[mask_ix] = fill_value
    endif
    
    writeu, energy_unit, initialgrid
  endfor  ; strips
  nrs_log_line, logfile, 'Finished solar radiation calculation for ' + demname, /append
  nrs_log_line, logfile, 'Total time: ' + nrs_sec_to_string(systime(/seconds) - t1, /time, /hours24), /append
  
  close, energy_unit
  free_lun, energy_unit

  envi_file_mng, id = fid_coslope, /remove
  envi_file_mng, id = fid_sincos, /remove
  envi_file_mng, id = fid_sinsin, /remove
  envi_file_mng, id = fid_slope, /remove

  inherit = envi_set_inheritance(dem, dims, /full)
  
  dt = size(initialgrid, /type)
  envi_setup_head, fname = energy $
        , /write $
        , data_type = dt $
        , ns = ns, nl = nl, nb = 1 $
        , interleave = 0 $
        , bnames = ['Solar radiation'] $
        , map_info = mi $
        , data_ignore_value = fill_value

end

;+
; :description:
;    Determine the pixel size in meters for a geographic location, assuming UTM with WGS-84 datum
;
; :params:
;    lat : in, required
;      The latitude in degrees; expected range [-90, 90] negative values indicating southern hemispere
;    lon : in, required
;      The longitude in degrees; expected range [-180, 180] negative values indicating western hemisphere
;    ps_ll : in, required
;      The pixel size in degrees as [lon_size, lat_size] 
;
; :keywords:
;    utmx : out, optional
;    utmy : out, optional
;      X and Y coordinates used to calculate the pixel size in meters
;
; :returns:
;   The pixel size in meters as [x_size, y_size]
;
; :author: nieuwenhuis
; :history:
;   march 2013: created
;-
function nrs_UTM_pixelsize_from_LL, lat, lon, ps_ll, utmx=utmx, utmy=utmy
  compile_opt idl2, logical_predicate, hidden
  
  zone = (floor((lon + 180) / 6) mod 60) + 1
  ll_proj = envi_proj_create(/geographic)
  utm_proj = envi_proj_create(/utm, zone = zone, south = (lat lt 0), datum = 'WGS-84')
  xx = [lon, lon + ps_ll[0]]
  yy = [lat, lat + ps_ll[1]]
  envi_convert_projection_coordinates, xx, yy, ll_proj, utmx, utmy, utm_proj
  ps = [utmx[1] - utmx[0], utmy[1] - utmy[0]]
  
  return, [ps]
end
 
function nrs_solar_prepare_slap_maps, demname, swap_aspect = swap_aspect
  compile_opt idl2, logical_predicate
  
  outname = getOutname(demname, postfix = '_slap', ext = '.dat')
  outcoslope = getOutname(demname, postfix = '_cosslp', ext = '.dat')
  outsincos = getOutname(demname, postfix = '_cossin', ext = '.dat')
  outsinsin = getOutname(demname, postfix = '_sinsin', ext = '.dat')
  
  ; test if we already have the slope maps
  fi_out = file_info(outname)
  fi_cosl = file_info(outcoslope)
  fi_sc = file_info(outsincos)
  fi_ss = file_info(outsinsin)
  
  deg2rad = !dpi / 180
  
  ; swap_aspect = -1  : swap north for south and change from clockwise to counter clockwise
  ; swap_aspect =  1   ; keep north where it is
  swap_aspect = keyword_set(swap_aspect) ? -1 : 1
  
  envi_open_file, demname, r_fid = dem, /no_realize, /no_interactive_query
  envi_file_query, dem, ns = ns, nl = nl, dims = dims, data_ignore_value = undef
  mi = envi_get_map_info(fid = dem, undefined = csy_undef)
  
  envi_convert_file_coordinates, dem, 0.0, 0.0, x_left, y_top, /to_map
  envi_convert_file_coordinates, dem, ns, nl, x_right, y_bot, /to_map
  lat = (y_top + y_bot) / 2
  lon = (x_left + x_right) / 2
  ps_m = nrs_UTM_pixelsize_from_LL(lat, lon, mi.ps)

  if fi_out.exists then begin
    envi_open_file, outname, r_fid = demslop, /no_realize, /no_interactive_query
  endif else begin
    ; calculate slope (in degrees) and aspect
    envi_doit, 'topo_doit', fid = dem $
                , /no_realize $
                , bptr = [0, 1] $
                , dims = dims $
                , out_name = outname $
                , out_bname = ['Slope', 'Aspect'] $
                , pixel_size = ps_m $
                , pos = [0] $
                , azi = 45, elev=45 $
                , kernel = 3 $
                , r_fid = demslop 
  endelse

  if ~(fi_cosl.exists && fi_sc.exists && fi_ss.exists) then begin 
    inherit = envi_set_inheritance(dem, dims, /full)
    if undef eq 1.0e-34 then void = temporary(undef)
    doMask = (n_elements(undef) gt 0)
    
    openw, f_coslope, outcoslope, /get_lun
    openw, f_sincos, outsincos, /get_lun
    openw, f_sinsin, outsinsin, /get_lun
  
    nr_steps = (ns * nl) / 10000000L + 1  ; 10 M values
    line_step = nl / nr_steps + 1
    sl = 0
    el = line_step - 1
    mask_ix = []
    cnt_undef = 0
    for s = 0, nr_steps - 1 do begin
      dims = [-1, 0, ns - 1, sl, el]
      sl = el + 1
      el = min([nl - 1, el + line_step])
      if doMask then begin
        elev = envi_get_data(fid = dem, dims = dims, pos = [0])
        mask_ix = where(elev eq undef, cnt_undef)
      endif
      slope = envi_get_data(fid = demslop, dims = dims, pos = [0]) * deg2rad ; slope to radians
      aspect = envi_get_data(fid = demslop, dims = dims, pos = [1]) * deg2rad ; aspect to radians
      
      ix = where(aspect lt 0, count)
      if count gt 0 then aspect[ix] = 0.0 ; handle flat spots
      
      sinslope = sin(slope)
      cosslope = cos(slope)
      sinaspect = sin(aspect)
      cosaspect = cos(aspect) * swap_aspect
      sincos = sinslope * cosaspect
      sinsin = sinslope * sinaspect
  
      if doMask && (cnt_undef gt 0) then begin
        cosslope[mask_ix] = undef
        sincos[mask_ix] = undef
        sinsin[mask_ix] = undef
      endif
      writeu, f_coslope, cosslope 
      writeu, f_sincos, sincos 
      writeu, f_sinsin, sinsin 
    endfor
    close, f_coslope
    close, f_sincos
    close, f_sinsin
    free_lun, f_coslope
    free_lun, f_sincos
    free_lun, f_sinsin
    
    envi_file_mng, id = demslop, /remove
    
    dt = size(cosslope, /type)
    envi_setup_head, fname = outcoslope $
          , /write $
          , data_type = dt $
          , ns = ns, nl = nl, nb = 1 $
          , interleave = 0 $
          , bnames = ['cos(slope)'] $
          , map_info = mi $
          , data_ignore_value = undef
    
    envi_setup_head, fname = outsincos $
          , /write $
          , data_type = dt $
          , ns = ns, nl = nl, nb = 1 $
          , interleave = 0 $
          , bnames = ['sin(slope)*cos(aspect)'] $
          , map_info = mi $
          , data_ignore_value = undef
  
    envi_setup_head, fname = outsinsin $
          , /write $
          , data_type = dt $
          , ns = ns, nl = nl, nb = 1 $
          , interleave = 0 $
          , bnames = ['sin(slope)*sin(aspect)'] $
          , map_info = mi $
          , data_ignore_value = undef
  
  endif ; ~(fi_cosl.exists && fi_sc.exists && fi_ss.exists)
  
  ; return the names of the intermediate products
  return, [outcoslope, outsincos, outsinsin, outname]

end

pro swr_stat, swr, asp, tab
  envi_file_query, swr, dims=dims, ns=ns, nl=nl
  swr_data = envi_get_data(fid=swr, dims=dims, pos=[0])
  aspec = envi_get_data(fid=asp,dims=dims,pos=[0])
  n_ix = where(aspec eq 1 or aspec eq 8,n_cnt)
  e_ix = where(aspec eq 2 or aspec eq 3,n_cnt)
  s_ix = where(aspec eq 4 or aspec eq 5,n_cnt)
  w_ix = where(aspec eq 6 or aspec eq 7,n_cnt)
  
  n_m = moment(swr_data[n_ix])
  e_m = moment(swr_data[e_ix])
  s_m = moment(swr_data[s_ix])
  w_m = moment(swr_data[w_ix])
  all = reform([n_m, e_m, s_m, w_m],4,4)
  
  write_csv, 'E:\NRS\Jiang Yanbing\Solar radiation model\swr.csv' $
           , header=['dir','mean', 'var','skew','kurtosis'] $
           , ['north','east','south','west'] $
           , all[0,*] $
           , all[1,*] $
           , all[2,*] $
           , all[3,*]
  
end

