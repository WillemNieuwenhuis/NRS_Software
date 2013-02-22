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
;-
pro nrs_shortwaverad, dem, start_day, end_day, interval, output_name = energy
    pi = !dpi
    deg2rad = pi / 180
    time = interval * 2 * pi / (24 * 60) ; convert minutes into radians

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
    
    outname = getOutname(demname, postfix = '_slap')
    ; calculate slope (in degrees) and aspect
    envi_doit, 'topo_doit', fid = dem $
                , /no_realize $
                , bptr = [0, 1], dims = dims $
                , out_bname = ['Slope', 'Aspect'] $
                , pixel_size = mi.ps $
                , pos = [0] $
                , /in_memory $
                , r_fid = demslop 
    
    if demslop eq -1 then return
    
    slope = envi_get_data(fid = demslop, dims = dims, pos = [0]) * deg2rad ; slope to radians
    aspect = envi_get_data(fid = demslop, dims = dims, pos = [1])
    envi_file_mng, id = demslop, /remove
    
    ix = where(aspect lt 0, count)
    if count gt 0 then aspect[ix] = 0.0
    ix = where(aspect le 180, count)
    if count gt 0 then aspect[ix] = 180.0 - aspect[ix]
    ix = where(aspect gt 180, count)
    if count gt 0 then aspect[ix] = 540.0 - aspect[ix]
    aspect *= deg2rad
    
    
    initialgrid = fltarr(ns, nl)
    for strip = 0, nr_strips - 1 do begin
      s_y = strip * dim_ystep
      e_y = min([s_y + dim_ystep - 1, nl - 1])
      dims_ut = [-1, 0, ns - 1, s_y, e_y]
      nl_cur = e_y - s_y + 1
      nl_center = (e_y + s_y) / 2
       
      envi_convert_file_coordinates, dem, ns / 2, nl_center, londeg, latdeg, /to_map
      latitude = latdeg * deg2rad

      sinslope = sin(slope[*, s_y:e_y])
      cosslope = cos(slope[*, s_y:e_y])
      sinaspect = sin(aspect[*, s_y:e_y])
      cosaspect = cos(aspect[*, s_y:e_y])
      
      daynumber = start_day
      while daynumber le end_day do begin
        io = 1.367 * (1 + 0.034 * cos(2 * pi * daynumber / 365))  ; eq 7
        decl = 23.45 * deg2rad * sin(2 * pi * (284 + daynumber) / 365) ; eq 4
        
        ; eq 6
        ; (this needed to be adjusted for latitudes exceeding 66.5° N/S)
        if (-1 * tan(latitude) * tan(decl)) lt -1 then      $ ; check that x is not > 1.0 for acos(x)
          sunrise = acos(-1)                                $ ;             "
        else if (-1 * tan(latitude) * tan(decl)) gt 1 then  $ ;             "
          sunrise = acos(1)                                 $ ;             "
        else                                                $ ;             "
          sunrise = acos(-1 * tan(latitude) * tan(decl))      ; valid cases
  
        sunset = -1 * sunrise
      
        ; ensure calculations are at half the time interval
        hourangle = sunrise - (time / 2)
        
        ; extract individual terms of the equation to speed up calculations
        ;        cosi = cosi1 + cos(decl) * cos(hourangle) * cosi2 + cosi3 * sin(hourangle)
  
        ; terms of the cosi equation which do not depend on the inner loop
        cosi1 = sin(decl) * (sin(latitude) * cosslope - cos(latitude) * sinslope * cosaspect)
        cosi2 = cos(latitude) * cosslope + sin(latitude) * sinslope * cosaspect
        cosi3 = cos(decl) * sinslope * sinaspect
  
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
  
           wattsgrid = iso * cosi * sungrid * shaded * 60 * interval
           initialgrid[*, s_y:e_y] = wattsgrid + initialgrid[*, s_y:e_y]
  
           hourangle = hourangle - time
        endwhile
        daynumber++
      endwhile
    endfor  ; strips
    
    outgrid = initialgrid
    
    if n_elements(energy) eq 0 then begin
      outname = getOutname(demname, postfix = '_rad', ext = '.')
    endif else begin
      outname = energy
    endelse
    
    envi_write_envi_file, outgrid, out_name = outname, dims = dims, map_info = mi
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
