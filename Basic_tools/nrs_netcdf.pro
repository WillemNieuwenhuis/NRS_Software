;+
; :Description:
;    Get a (subselection of) the attributes of the netCDF variable
;
; :Params:
;    nc_id :
;      The handle of the open netCDF file
;    var_id :
;      The ID of the variable
;
; :Keywords:
;    desc : out
;      The description of the variable; it will use the long_name attribute; if this is not found
;      the standard_name attribute is use; if this is also not present the variable name is used
;    grid_mapping : out
;      The grid_mapping; if not there an empty string is returned
;    units : out
;      The unit string; in case of degreey_xxx strings: this is parsed and the only string returned
;      is one of 'degrees_north', 'degrees_east', degrees_south', 'degrees_west'
;    missing_value : out
;      The missing value as specified by the _FillValue attribute or the missing_value attribute,
;      whichever is available. No distinction is made between the two attributes. If both 
;      attributes do no occur, the value will be undefined
;    crd_str : out
;      A string with the value of the coordinates attribute; if not present an empty string is returned
;    bnds_str : out
;      A string with value of the bounds attributes; if not present an empty string is returned
;    pole_lat : out
;      The value of the grid_north_pole_latitude attribute (grid_mapping var); if the attribute
;      is not present, the value will be undefined
;    pole_lon : out
;      The value of the grid_north_pole_longitude attribute (grid_mapping var); if the attribute
;      is not present, the value will be undefined
;    scale_factor : out
;      The value of the scale_factor attribute; if the attribute is not present, the
;      value will be undefined
;    add_offset : out
;      The value of the add_offset attribute; if the attribute is not present, the
;      value will be undefined
;
; :Author: nieuwenhuis
;-
pro nrs_nc_get_varatt, nc_id, var_id, desc = desc, grid_mapping = grid_mapping, units = units $
                                    , missing_value = missing_value $
                                    , crd_str = crd_str $
                                    , bnds_str = bnds_str $
                                    , pole_lat = pole_lat, pole_lon = pole_lon $
                                    , scale_factor = scale_factor, add_offset = add_offset
  compile_opt idl2, logical_predicate

  desc = ''
  grid_mapping = ''
  units = ''
  v_att = ncdf_varinq(nc_id, var_id)
  if v_att.natts gt 0 then begin
    for i = 0, v_att.natts - 1 do begin
      attname = ncdf_attname(nc_id, var_id, i)
      ncdf_attget, nc_id, var_id, attname, value
      an = strlowcase(attname)
      case an of
        'long_name' : desc = string(value)
        'standard_name' : if strlen(desc) eq 0 then desc = string(value)
        'grid_mapping' : grid_mapping = string(value)
        'units' : units = nrs_nc_parse_units(string(value))
        'missing_value' : missing_value = float(value)
        '_fillvalue' : missing_value = float(value)
        'scale_factor' : scale_factor = float(value)
        'add_offset' : add_offset = float(value)
        'coordinates' : crd_str = string(value)
        'bounds' : bnds_str = string(value)
        'grid_north_pole_latitude' : pole_lat = float(value)
        'grid_north_pole_longitude' : pole_lon = float(value)
      else :
      endcase
    endfor
  endif
  
  if strlen(desc) eq 0 then desc = v_att.name
end

function nrs_nc_parse_units, unit_str
  compile_opt idl2, logical_predicate
  
  switch unit_str of
    'degree_n' : 
    'degree_north' :
    'degrees_n' : 
    'degrees_north' : return, 'degrees_north'
    'degree_e' : 
    'degree_east' : 
    'degrees_e' : 
    'degrees_east' : return, 'degrees_east' 
    'degree_w' : 
    'degree_west' :
    'degrees_w' : 
    'degrees_west' : return, 'degrees_west'
    'degree_s' : 
    'degree_south' : 
    'degrees_s' : 
    'degrees_south' : return, 'degrees_east'
  endswitch
  
  return, unit_str
end
 
function nrs_datatype_from_string, datatype_str
  compile_opt idl2, logical_predicate
  
  all_dts = ['byte', 'char', 'int', 'long', 'float', 'double']
  all_dt  = [1, 1, 2, 3, 4, 5]
  
  ix = where(all_dts eq strlowcase(datatype_str), cnt)
  
  return, cnt eq 0 ? 4 : all_dt[ix[0]]
end

;+
; :Description:
;    Parse the netCDF file to find the variables that are potentially data variables
;
; :Params:
;    nc_id :
;      The handle of the open netCDF file
;
; :Keywords:
;    grid_mapping : out
;      A list of variables containing gridmapping info
;    crd_vars : out
;      The list of possible data variables
;
; :Author: nieuwenhuis
;-
function nrs_nc_get_var_cand, nc_id, grid_mapping = gm_ids, crd_vars = crd_vars
  compile_opt idl2, logical_predicate

  crd_vars = { coord $
        , name : string('') $
        , did  : -1 $   ; dimension id
        , vid  : -1 $   ; coord variable id
      }
  plt_var = { map_var $
        , vid  : -1 $
        , name : string('') $
        , gm : string('') $  ; grid_mapping, if any
        , unit : string('') $
        , dim_ids : intarr(4) $
        , data_type : 4 $ ; 4 == float
        , x_id : -1 $
        , y_id : -1 $
        , z_id : -1 $
        , t_id : -1 $
        , px_id : -1 $   ; projected x coord, in case of grid_mapping
        , py_id : -1 $    ; projected y coord, in case of grid_mapping
        , ns : 0 $
        , nl : 0 $
        , nz : 0 $
        , nb : 0 $
      }

  nc_vars = ncdf_inquire(nc_id)
  
  ; get the dimensions
  dim_names = strarr(nc_vars.ndims)
  dim_size = intarr(nc_vars.ndims)
  dim_ids = intarr(nc_vars.ndims)
  unlim = nc_vars.recdim
  for d = 0, nc_vars.ndims - 1 do begin
    ncdf_diminq, nc_id, d, name, size
    dim_names[d] = name
    dim_size[d] = size
    dim_ids[d] = d
  endfor
  
  nvars = nc_vars.nvars 
  crd_vars = []
  var_ids = []
  var_names = []
  gm_ids = []
  for id = 0, nvars - 1 do begin
    varstruct = ncdf_varinq(nc_id, id)
    name = varstruct.name
    ndim = varstruct.ndims
    dims = varstruct.dim
    dt = varstruct.datatype
    case ndim of
      0 : gm_ids = [gm_ids, id] 
      1 : if name eq dim_names[dims[0]] then begin
            crd_vars = [crd_vars, {coord, name:name, did:dims[0], vid:id} ]
          endif
    else :  begin
        new_var = plt_var
        new_var.name = name
        new_var.vid = id
        new_var.data_type = nrs_datatype_from_string(dt)
        nd = min([ndim, 4])
        new_var.dim_ids[0 : nd - 1]  = dims[0 : nd - 1]
        var_ids = [var_ids, new_var]
      end
    endcase
  endfor
  for v = 0, n_elements(var_ids) - 1 do begin
    dims = var_ids[v].dim_ids
    
    ; time coordinate
    if nd ge 3 then begin
      ix = where(crd_vars.did eq dims[nd - 1], cnt)
      if cnt gt 0 then begin
        var_ids[v].t_id = crd_vars[ix[0]].vid
        ncdf_diminq, nc_id, dims[nd - 1], name, size
        var_ids[v].nb = size
      endif
    endif
    ; a 3D coordinate if available
    if nd ge 4 then begin
      ix = where(crd_vars.did eq dims[2], cnt)
      if cnt gt 0 then begin
        var_ids[v].z_id = crd_vars[ix[0]].vid
        ncdf_diminq, nc_id, dims[2], name, size
        var_ids[v].nz = size
      endif
    end
    ; x and x coordinates
    ix = where(crd_vars.did eq dims[1], cnt)
    if cnt gt 0 then begin
      var_ids[v].y_id = crd_vars[ix[0]].vid
      ncdf_diminq, nc_id, dims[1], name, size
      var_ids[v].nl = size
    endif
    ix = where(crd_vars.did eq dims[0], cnt)
    if cnt gt 0 then begin
      var_ids[v].x_id = crd_vars[ix[0]].vid
      ncdf_diminq, nc_id, dims[0], name, size
      var_ids[v].ns = size
    endif
  endfor
  
  return, var_ids
end

;+
; :Description:
;    Parse the netCDF file to find the data variables
;
; :Params:
;    nc_id :
;      The handle of the open netCDF file
;
; :Keywords:
;    cancelled : out
;      Indicate that some error occured and the import will be stopped
;
; :Author: nieuwenhuis
;-
function nrs_nc_get_data_vars, nc_id, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  var_ids = nrs_nc_get_var_cand(nc_id, grid_mapping = gm_ids, crd_vars = crd_vars)
  
  ; find coordinate axis for the variables (spatial)
  for i = 0, n_elements(var_ids) - 1 do begin
    nrs_nc_get_varatt, nc_id, var_ids[i].vid, crd_str = crd_str, bnds_str = bnds_str
    if n_elements(crd_str) gt 0 then begin  ; parse coordinates attribute
      parts = strsplit(crd_str, /extract, count = pcnt)
      if pcnt eq 2 then begin
        crd_dep = parts
        ix = where(var_ids.name eq crd_dep, cnt)
        if cnt eq 2 then begin
          var_ids[i].px_id = var_ids[ix[0]].vid
          var_ids[i].py_id = var_ids[ix[1]].vid
        endif
      endif
    endif
  endfor

  ; find coordinate bounds for the variables 
  for i = 0, n_elements(crd_vars) - 1 do begin
    nrs_nc_get_varatt, nc_id, crd_vars[i].vid, bnds_str = bnds_str
    if n_elements(bnds_str) gt 0 then begin  ; temporal
      parts = strsplit(bnds_str, /extract, count = bcnt)
      if bcnt eq 1 then bnds_dep = parts
    endif
  endfor
  
  ; check if the coordinate variables are also listed as regular variables
  ; if so remove them as variables
  ; Spatial
  if n_elements(crd_dep) eq 2 then begin
    ix = where(var_ids.name eq crd_dep, cnt)
    nix = setdifference(indgen(n_elements(var_ids.name)), ix)
    if n_elements(nix) eq 0 then begin
      ans = dialog_message('No data found')
      return, []
    endif
    var_ids = var_ids[nix]
  endif
  ; Temporal
  if n_elements(bnds_dep) eq 1 then begin
    ix = where(var_ids.name eq bnds_dep, cnt)
    nix = setdifference(indgen(n_elements(var_ids.name)), ix)
    if n_elements(nix) gt 0 then begin
      var_ids = var_ids[nix]
    endif
  endif
  
  cancelled = 0
  
  return, var_ids
end

;+
; :Description:
;    Display an error message, and close resources
;
; :Params:
;    nc_id  :
;     the netCDF to close
;    msg :
;      The message to display
;
; :Keywords:
;    title :
;      The title of the message dialog
;    cancelled :
;      Indicate that the process is to be stopped
;
; :Author: nieuwenhuis
;-
pro nrs_nc_error_message, nc_id, msg, title = title, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  ans = dialog_message(msg, title = title, /error)
  ncdf_close, nc_id
  cancelled = 1
end

;+
; :Description:
;    Import a single netCDF file. It parses the netCDF file to find all data variables.
;    For each data variable an output file is created. 
;
; :Params:
;    filename :
;      The netCDF input file
;
; :Keywords:
;    out_name : in, optional
;      The basename of the output file(s). If not specified the input name will be used as template
;    prog_obj : in, optional
;      A progress indicator (of type progressBar)
;    cancelled : out
;      Indicates that the import is interrupted
;
; :Author: nieuwenhuis
;-
pro nrs_nc_get_data, filename, out_name = base_name $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  if n_elements(base_name) eq 0 then base_name = getOutname(filename, postfix = '_imp', ext = '.dat')
  
  nc_id = ncdf_open(filename)

  nrs_set_progress_property, prog_obj, /start, title = 'Importing ' + file_basename(filename)
    
  vars = nrs_nc_get_data_vars(nc_id, cancelled = cancelled)
  
  for v = 0, n_elements(vars) - 1 do begin
    var = vars[v]
    
    ; get time info
    if var.t_id ge 0 then begin
      nrs_nc_get_varatt, nc_id, var.t_id, units = time_units
      if n_elements(time_units) eq 0 then begin
        nrs_nc_error_message, nc_id, 'No time coordinate found, skipping ' + var.name $
                      , cancelled = cancelled, title = 'Import netCDF'
        continue
      endif
      ncdf_varget, nc_id, var.t_id, timar
      isCF = nrs_get_dt_from_units(timar, time_units, julian = julian, interval = interval)
     
      nrs_get_days_indices, julian, dmbands = dmbands, dybands = dybands, doy = doy
      dmbands = fix(total(dmbands, /cum)) - 1
      dybands = fix(total(dybands, /cum)) - 1
      caldat, julian, mm, dd, yy, hh, mn, ss
    endif
  
    ; now spatial extent
    nrs_set_progress_property, prog_obj, title = 'Loading coordinates'
    ns = var.ns
    nl = var.nl
    nb = var.nb
    nz = var.nz
    if nz gt 1 then begin
      nrs_nc_error_message, nc_id, 'No 3D data cube, skipping ' + var.name $
                    , cancelled = cancelled, title = 'Import netCDF'
      continue
    endif
    ncdf_varget, nc_id, var.y_id, lats
    ncdf_varget, nc_id, var.x_id, lons
    min_lat = min(lats, max = max_lat)
    min_lon = min(lons, max = max_lon)
    need_hor_mirror = (lats[n_elements(lats) - 1] - lats[0]) gt 0
  
    mc = [0.0, 0.0, min_lon, max_lat]
    ps = [(max_lon - min_lon) / (ns - 1), (max_lat - min_lat) / (nl - 1)]
    mi = envi_map_info_create(/geographic, mc = mc, ps = ps)
    
    nrs_set_progress_property, prog_obj, title = 'Converting ...'
    nrs_nc_get_varatt, nc_id, var.vid $
                     , units = unit_desc, missing_value = nodata, desc = var_desc $
                     , grid_mapping = gm $
                     , scale_factor = scale_factor, add_offset = offset
    nodata_set = n_elements(nodata) gt 0

    postfix = '_' + var.name
    output_name = getOutname(base_name, postfix = postfix, ext = '.dat')
  
    dt = var.data_type
    if n_elements(scale_factor) gt 0 then dt = size(scale_factor, /type) $
    else if n_elements(offset) gt 0 then dt = size(offset, /type)
  
    ; open the output for writing
    openw, unit, output_name, /get_lun

    buf = make_array(ns, nl, type = dt)
    var_cnt = [ns, nl]
    var_offset = [0, 0]
    if nb gt 0 then begin
      var_cnt = [ns, nl, 1]
      var_offset = [0, 0, 0]
    endif
    if nz eq 1 then begin
      var_cnt = [ns, nl, 1, 1]
      var_offset = [0, 0, 0, 0]
    endif
    nb = nb eq 0 ? 1 : nb 
    for b = 0, nb - 1 do begin
      if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return

      var_offset[-1] = b
      ncdf_varget, nc_id, var.vid, band, count = var_cnt, offset = var_offset  ; read next band
      if nz eq 1 then band = reform(band, ns, nl, /overwrite)
      
      if nodata_set then ndix = where(band eq nodata, nd_cnt)
      do_scale = n_elements(scale_factor) gt 0 && abs(1.0 - scale_factor) gt 0.001
      do_offset = n_elements(offset) gt 0 && abs(0.0 - offset) gt 0.001 
      if do_scale || do_offset then begin
        if do_scale then band *= scale_factor
        if do_offset then band += offset
        if nd_cnt gt 0 then band[ndix] = nodata
      endif
      
      ; check for 2D-coord vars
      if var.px_id ge 0 && var.py_id ge 0 then begin
        if strlen(gm) gt 0 then begin
          gm_id = ncdf_varid(nc_id, gm)
          
          nrs_nc_get_varatt, nc_id, gm_id, pole_lat = pole_lat, pole_lon = pole_lon

          ; only rotate / resample if needed
          if n_elements(pole_lat) gt 0 then begin
            if ~(abs(pole_lat - 90) lt 0.000001) then begin
              ncdf_varget, nc_id, var.px_id, lons 
              ncdf_varget, nc_id, var.py_id, lats 
              resam = make_array(ns, nl, type = dt)
            endif
          endif
        endif
      endif
      
      ; if pixels run from bottom to top, swap them
      if need_hor_mirror then begin
        for l = 0, nl - 1 do buf[*, l] = band[*, nl - l - 1]
        band = buf
      endif
      
      writeu, unit, band   ; write to disk
    endfor  ; b 
      
    ; build band names
    bnames = var.name
    if nb gt 1 then begin
      if (interval ge 28 * 86400D) and (interval le 31 * 86400D) then begin
        bnames = 'Year.month ' + string(yy, format = '(I04)') + '.' + string(mm, format = '(I02)')
      end else begin
        if interval ge 86400D then begin
          bnames = 'Year.day ' + string(yy, format = '(I04)') + '.' + string(doy, format = '(I03)')
        endif else begin
          bnames = 'Year.day ' + string(yy, format = '(I04)') + '.' + string(doy, format = '(I03)') $
                               + string(hh, format = '("; ",I02)')+ string(mn, format = '(":",I02)')+ string(ss, format = '(":",I02)')
        endelse
      endelse
    endif

    close, unit
    free_lun, unit

    envi_setup_head, fname = output_name $
            , descrip = var_desc $
            , data_type = dt $
            , /write $
            , interleave = 0 $  ; BSQ
            , nb = nb, nl = nl, ns = ns $
            , bnames = bnames $
            , map_info = mi $
            , data_ignore_value = nodata
  
  endfor ; v
  
  ncdf_close, nc_id  ; done reading; close the nc file
end

function nrs_yyyymmdd_to_julian, ymd
  compile_opt idl2, logical_predicate
  
  y = fix(ymd / 10000)
  m = fix((ymd - y * 10000L) / 100)
  d = fix(ymd - y * 10000L - m * 100)
  t = ymd - y * 10000L - m * 100 - d
  m = m > 1
  d = d > 1
  
  return, julday(m, d, y) + t
end

;+
; :Description:
;    Calculate the start date and end date of the start and end offsets in domain of time_units 
;
; :Returns:
;   <ul>
;   <li> 0 (zero) the conversion failed
;   <li> 1        the conversion was succesful for CF compliant file
;   <li> 2        the conversion was succesful for non-CF compliant file
;   </ul>
;    
; :Params:
;    timar : in, required
;      Array with numerical value indicating all dates and time
;    time_units : in, required
;      the string defining the interval and absolute startdate. Min_date and max_date
;      parameters are defined as the number of intervals since the absolute startdate.
;      Time_units is a string, formatted as 'days since 1950-01-01 00:00:00'
;      As a cludge it also supports 'day as %Y%m%d.%f'
;
; :Keywords:
;    julian : out
;      array containing the date/time of the bands in julian days
;    interval : out
;      time resolution of the netCDF file in seconds
;
; :Author: nieuwenhuis
;-
function nrs_get_dt_from_units, timar, time_units, julian = julian, interval = interval
  compile_opt idl2, logical_predicate
  
  ; first determine the absolute start date and the time interval
  periods = ['yr', 'year', 'years', 'mon', 'month', 'months', 'week', 'weeks', 'd', 'day', 'days', 'h', 'hr', 'hour', 'hours', 'min', 'minute', 'minutes', 's', 'sec', 'seconds']
  piv = [-365, -365, -365, -31,-31, -31, -7, -7, 86400, 86400, 86400, 3600, 3600, 3600, 3600, 60, 60, 60, 1, 1, 1]
  parts = strsplit(time_units, ' ', /extract)
  
  min_date = min(timar, max = max_date)
  if n_elements(parts) ge 3 then begin
    day = 1
    month = 1
    year = 1
    hour = 0
    minute = 0
    second = 0
    ix = where(periods eq parts[0], cix)
    if cix eq 0 then return, 0
    
    interval = piv[ix[0]]
    if parts[1] eq 'since' then begin
      ymd = strsplit(parts[2], '-', /extract)
      ymd_cnt = n_elements(ymd)
      if ymd_cnt eq 0 then return, 0
      
      switch ymd_cnt of
        3 : day = fix(ymd[2])
        2 : month = fix(ymd[1]) 
        1 : year = fix(ymd[0])
      endswitch
      if n_elements(parts) ge 4 then begin
        hms = strsplit(parts[3], ':', /extract)
        hms_cnt = n_elements(hms)
        switch hms_cnt of
          3 : second = fix(hms[2])
          2 : minute = fix(hms[1])
          1 : hour = fix(hms[0])
        endswitch
      endif
      jd = julday(month, day, year, hour, minute, second)
      if interval lt 0 then begin
        case interval of
          -365 : julian = julday(month, day, year + timar, hour, minute, second)
          -31 : julian = julday(((month + timar - 1) mod 12) + 1, day, year + (month + timar - 1) / 12, hour, minute, second)
          -7 : julian = (double(interval) * timar * 7) / 86400D + jd
        endcase
      endif $
      else julian = (double(interval) * timar) / 86400D + jd
      
      return, 1
    endif else if parts[1] eq 'as' then begin
      ; unit:day as %Y%m%d.%f
      ;   assume %4d%2d%2d.%0.f
      julian = nrs_yyyymmdd_to_julian(timar)
      
      if n_elements(timar) eq 1 then interval = 0 $
      else  interval = (julian[1] - julian[0]) * 86400D
      
      return, 2 ; indicate non CF compliant
    endif
    
  endif
  
  return, 0
end

;+
; :Description:
;    Import all netCDF files in a folder
;
; :Params:
;    The folder where the netCDF file are located
;
; :History:
;   code changes::
;     september 2011 : First version
;     februari 2012 : Generalized the import; it will now determine the data variable by itself
;
; :Author: nieuwenhuis
;-
pro nrs_nc_import, folder
  compile_opt idl2, logical_predicate

  ; outer progress indicator  
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Importing netCDF files to ENVI' $
                        , /fast_loop $
                        )

  progressBar->Start

  ; inner progress indicator
  progressInner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Importing netCDF files to ENVI' $
                        , level = 1 $
                        , /fast_loop $
                        )

  files = nrs_find_images(folder, '.*', extension = 'nc')
  if n_elements(files) gt 0 && size(files[0], /type) eq 7 then begin
    for f = 0, n_elements(files) - 1 do begin
      if nrs_update_progress(progressBar, f, n_elements(files)) then return

      nrs_nc_get_data, files[f], prog_obj = progressInner, cancelled = cancelled
    endfor
  endif

  if obj_valid(progressBar) gt 0 then progressBar -> Destroy
  if obj_valid(progressInner) gt 0 then progressInner -> Destroy
  
end
