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
;      Start day (in julian day)
;    end_day : in, required
;      End day (in julian days)
;    interval : in, required
;      Time interval in minutes (time)
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
;-
pro nrs_shortwaverad, dem, start_day, end_day, interval, output_name = energy
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
  
  lat_ext = y_bot - y_top
  nr_strips = ceil(abs(lat_ext))
  dim_ystep = 1 + nl / nr_strips
  
  intermediate = nrs_solar_prepare_slap_maps(demname, /swap_aspect)
  if n_elements(intermediate) ne 4 then begin
    void = error_message('Failed to calculate slope / aspect image', /error)
    return
  endif
  
  envi_open_file, intermediate[0], r_fid = fid_coslope, /no_realize, /no_interactive_query
  envi_open_file, intermediate[1], r_fid = fid_sincos, /no_realize, /no_interactive_query
  envi_open_file, intermediate[2], r_fid = fid_sinsin, /no_realize, /no_interactive_query
  
  if n_elements(energy) eq 0 then begin
    outname = getOutname(demname, postfix = '_rad', ext = '.dat')
  endif else begin
    outname = energy
  endelse

  openw, energy_unit, outname, /get_lun
  
  initialgrid = fltarr(ns, dim_ystep)
  for strip = 0, nr_strips - 1 do begin
    s_y = strip * dim_ystep
    e_y = min([s_y + dim_ystep - 1, nl - 1])
    if (e_y - s_y + 1) ne dim_ystep then $
      initialgrid = fltarr(ns, e_y - s_y + 1) ; special case: last part can be smaller
    dims_ut = [-1, 0, ns - 1, s_y, e_y]
    nl_cur = e_y - s_y + 1
    nl_center = (e_y + s_y) / 2
     
    envi_convert_file_coordinates, dem, ns / 2, nl_center, londeg, latdeg, /to_map
    latitude = latdeg * deg2rad

    cosslope = envi_get_data(fid = fid_coslope, dims = dims_ut, pos = [0])
    sincos = envi_get_data(fid = fid_sincos, dims = dims_ut, pos = [0])
    sinsin = envi_get_data(fid = fid_sinsin, dims = dims_ut, pos = [0])

    daynumber = start_day
    while daynumber le end_day do begin
print,strip,daynumber    
      io = 1.367 * (1 + 0.034 * cos(2 * pi * daynumber / 365))  ; eq 7
      decl = 23.45 * deg2rad * sin(2 * pi * (284 + daynumber) / 365) ; eq 4
      
      ; eq 6
      ; (adjusted for latitudes exceeding 66.5° N/S)
      tl_td = -1 * tan(latitude) * tan(decl)
      if tl_td lt -1 then sunrise = pi $
      else if tl_td gt 1 then sunrise = 0.0 $
      else sunrise = acos(tl_td)      ; regular cases

      sunset = -1 * sunrise
    
      ; ensure calculations are at half the time interval
      hourangle = sunrise - (time / 2)
      
      ; extract individual terms of the equation to speed up calculations
      ;        cosi = cosi1 + cos(decl) * cos(hourangle) * cosi2 + cosi3 * sin(hourangle)
      ; terms of the cosi equation which do not depend on the inner loop
      cosi1 = sin(decl) * (sin(latitude) * cosslope - cos(latitude) * sincos) ;sinslope * cosaspect)
      cosi2 = cos(latitude) * cosslope + sin(latitude) * sincos ;sinslope * cosaspect
      cosi3 = cos(decl) * sinsin ; sinslope * sinaspect

      while hourangle ge sunset do begin
         ; eq 2:
         solaralt = asin(sin(latitude) * sin(decl) + cos(latitude) * cos(decl) * cos(hourangle))
         test = tan(decl) / tan(latitude)
  
         ; eq 3:
         if (cos(hourangle) gt test) then                                  $
           solaraz = asin(cos(decl) * sin(hourangle) / cos(solaralt))      $
         else if (cos(hourangle) lt test) then                             $
           solaraz = pi - asin(cos(decl) * sin(hourangle) / cos(solaralt)) $
         else if (test eq cos(hourangle)) and (hourangle ge 0) then        $
           solaraz = pi / 2                                                $
         else if (test eq cos(hourangle)) and (hourangle lt 0) then        $
           solaraz = -1 * pi / 2
  
         if (solaraz ge 0) then                         $
           solarazdeg = solaraz / deg2rad               $
         else                                           $
           solarazdeg = 360 - (abs(solaraz) / deg2rad)

         ; eq 11:
         m = sqrt(1229 + (614 * sin(solaralt)) ^ 2) - 614 * sin(solaralt)
         
         ; eq 15:
         iso = io * 0.56 * (exp(-0.65 * m) + exp(-0.095 * m))
  
         solaraltdeg = solaralt / deg2rad
  
         if (solarazdeg le 180) then                    $  ; correct for northern lat.s
           azi = (180 - solarazdeg)                     $  ;        "
         else                                           $  ;        "
           azi = (180 + (360 - solarazdeg))                ;        "

         envi_doit, 'topo_doit', fid = dem $
                     , bptr = [2], dims = dims_ut $
                     , out_bname = ['Hillshade'] $
                     , azimuth = azi $
                     , elevation = solaraltdeg $
                     , pixel_size = mi.ps $
                     , pos = [0] $
                     , /in_memory $
                     , r_fid = shade 

         envi_file_query, shade, dims = dims_sh, nl = nl_cur
         sungrid = envi_get_data(fid = shade, dims = dims_sh, pos = [0])
         envi_file_mng, id = shade, /remove

         cosi = cosi1 + cos(decl) * cos(hourangle) * cosi2 + cosi3 * sin(hourangle)
         shaded = bytarr(ns, nl_cur)
         ix = where(cosi gt 0, count)
         if count gt 0 then shaded[ix] = 1

         initialgrid += iso * cosi * sungrid * shaded * 60 * interval

         hourangle = hourangle - time
      endwhile
      daynumber++
    endwhile
    
    writeu, energy_unit, initialgrid
  endfor  ; strips
  
  close, energy_unit
  free_lun, energy_unit

  envi_file_mng, id = fid_coslope, /remove
  envi_file_mng, id = fid_sincos, /remove
  envi_file_mng, id = fid_sinsin, /remove
;  envi_file_mng, id = dem, /remove

  inherit = envi_set_inheritance(dem, dims, /full)
  
;  envi_setup_head, fname = energy $
;    , data_type = 4 $ ; float
;    , ns = ns, nl = nl, nb = 1 $
;    , /write $
;    , inherit = inherit

    envi_setup_head, fname = energy $
        , /write $
        , data_type = 5 $
        , ns = ns, nl = nl, nb = 1 $
        , interleave = 0 $
        , bnames = ['Solar radiation'] $
        , map_info = mi ;$
;        , data_ignore_value = fill_value
  
end

function nrs_prepare_slap_maps, demname, swap_aspect = swap_aspect
  compile_opt idl2, logical_predicate
  
  ; swap_aspect = -1  : swap north for south and change from clockwise to counter clockwise
  ; swap_aspect =  1   ; keep north where it is
  swap_aspect = keyword_set(swap_aspect) ? -1 : 1
  
  outname = getOutname(demname, postfix = '_slap', ext = '.dat')
  outcoslope = getOutname(demname, postfix = '_cosslp', ext = '.dat')
  outsincos = getOutname(demname, postfix = '_cossin', ext = '.dat')
  outsinsin = getOutname(demname, postfix = '_sinsin', ext = '.dat')

  envi_open_file, demname, r_fid = dem, /no_realize, /no_interactive_query
  mi = envi_get_map_info(fid = dem, undefined = no_csy)
  
  ; calculate slope (in degrees) and aspect
  envi_doit, 'topo_doit', fid = dem $
              , /no_realize $
              , bptr = [0, 1], dims = dims $
              , out_name = outname $
              , out_bname = ['Slope', 'Aspect'] $
              , pixel_size = mi.ps $
              , pos = [0] $
              , r_fid = demslop 

  envi_file_query, demslop, ns = ns, nl = nl
  inherit = envi_set_inheritance(demslop, dims, /full)
  
  openw, f_coslope, outcoslope, /get_lun
  openw, f_sincos, outsincos, /get_lun
  openw, f_sinsin, outsinsin, /get_lun

  nr_steps = (ns * nl) / 10000000L + 1
  line_step = nl / nr_steps
  sl = 0
  el = line_step - 1
  for s = 0, nr_steps - 1 do begin
    dims = [-1, 0, ns, sl, el]
    sl = el + 1
    el = min([nl, el + line_step])
    slope = envi_get_data(fid = demslop, dims = dims, pos = [0]) * deg2rad ; slope to radians
    aspect = envi_get_data(fid = demslop, dims = dims, pos = [1]) * deg2rad ; slope to radians
    
    ix = where(aspect lt 0, count)
    if count gt 0 then aspect[ix] = 0.0 ; handle flat spots
    aspect *= deg2rad
    
    sinslope = sin(slope)
    cosslope = cos(slope)
    sinaspect = sin(aspect)
    cosaspect = cos(aspect) * swap_aspect
    sincos = sinslope * cosaspect
    sinsin = sinslope * sinaspect

    writeu, f_coslope, cosslope 
    writeu, f_sincos, sincos 
    writeu, f_sinsin, sinsin 
    
  endfor
  
  envi_setup_head, fname = outcoslope $
        , data_type = 4 $ ; float
        , ns = ns, nl = nl $
        , /write $
        , inherit = inherit
  
  envi_setup_head, fname = outsincos $
        , data_type = 4 $ ; float
        , ns = ns, nl = nl $
        , /write $
        , inherit = inherit

  envi_setup_head, fname = outsinsin $
        , data_type = 4 $ ; float
        , ns = ns, nl = nl $
        , /write $
        , inherit = inherit

  close, f_coslope
  close, f_sincos
  close, f_sinsin
  free_lun, f_coslope
  free_lun, f_sincos
  free_lun, f_sinsin
  
  ; return the names of the intermediate products
  return, [outcoslope, outsincos, outsinsin]

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
