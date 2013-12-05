function nrs_jul_to_band, jd, sd, ed, nb
  compile_opt idl2, logical_predicate
  
  if n_elements(jd) eq 0 then jd = sd
  
  return, (long(0.5 + nb * 1.0 * (jd - sd) / (ed - sd)) > 0) < (nb - 1)
end

function nrs_band_to_jul, band, sd, ed, nb
  compile_opt idl2, logical_predicate
  
  if n_elements(band) eq 0 then band = 0
  
  if size(sd, /type) eq 3 then $
    return, long(1.0 * band / nb * (ed - sd) + sd) $
  else $
    return, 1.0 * band / nb * (ed - sd) + sd
end

;+
; :description:
;    Calculate the correlation coefficient of the time series
;    in each location with a profile.
;    
;    A temporal susbset can be specified for the calculation; this can
;    be done as dates or as band numbers. If both are specified the band numbers
;    take preference
;
; :params:
;    image: in, required
;      The input time series
;    profile_table: in, required
;      The text table with the profile data, consisting of a date column
;      end one or more data columns
;
; :keywords:
;    start_date: in, required
;      The date associated with the start of the time series
;    end_date: in, required
;      The date associated with the end of the time series
;    from_date: in, optional, default = start_date
;      Start of the temporal subset to calculate the correlation as date
;    to_date: in, optional, default = end_date
;      End of the temporal subset to calculate the correlation as date
;    from_band: in, optional, default = first band in time series
;      Start of the temporal subset to calculate the correlation as band number
;    to_band: in, optional, default = last band in time series
;      End of the temporal subset to calculate the correlation as band number
;    ignore_NAN: in, optional, default = no
;      If specified and non-zero calculate the correlation ignoring NaN or the data ignore value
;    outname: in, optional
;      The name of the output file
;    prog_obj : in
;      ProgressBar object for displaying progress
;    cancelled : out
;      If set indicates failure or stopping of the progress by the user
;
; :author: nieuwenhuis
; 
; :history:
;   changes::
;     14 nov 2013: nieuwenhuis, created
;-
pro nrs_correlate_profile, image, profile_table $
                         , start_date = start_date $
                         , end_date = end_date $
                         , from_date = from_date $
                         , to_date = to_date $
                         , from_band = from_band $
                         , to_band = to_band $
                         , ignore_NAN = ignore_NAN $
                         , outname = outname $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  ; parameter checking
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, data_type = dt, dims = dims, data_ignore_value = undef
  per = nrs_get_period_from_range(sd, ed, nb, per_str = input_period)
  
  asc = nrs_read_csv(profile_table, header = header, date_sep = '-/', time_sep = ':')
  field_count = n_tags(asc)
  if field_count lt 2 then begin
    void = error_message('Missing data from table: date and value column required')
    return
  endif
  
  dates = nrs_str2julian(asc.field1)
  if (dates[0] ne sd) || (dates[-1] ne ed) then begin
    void = error_message('Image start and end dates don''t match table dates')
    return
  end
  
  prof_data = asc.field2
  if n_elements(prof_data) ne nb then begin
    void = error_message('Number of table rows doesn''t match number of bands')
    return
  end
  
  if (n_elements(from_band) gt 0) && (n_elements(to_band) gt 0) then begin
    from_band = (from_band > 0) < (nb - 1) 
    end_band = (to_band > 0) < (nb - 1)
    if (from_band ge to_band) then begin
      from_band = 0
      to_band = nb - 1
    endif
  endif else begin
    from_d = sd
    to_d = ed
    if (n_elements(from_date) gt 0) && (n_elements(to_date) gt 0) then begin
      t1 = nrs_str2julian(from_date)
      t2 = nrs_str2julian(to_date)
      if t2 gt t1 then begin
        from_d = t1
        to_d = t2
      endif
    endif
    from_band = nrs_jul_to_band(from_d, sd, ed, nb)
    to_band = nrs_jul_to_band(to_d, sd, ed, nb)
  endelse
  pos = lindgen(to_band - from_band + 1) + from_band
  
  if n_elements(outname) eq 0 then outname = getOutname(image, postfix = '_corr', ext = '.dat')
  
  ignore_NAN = keyword_set(ignore_NAN)
  ignore_undef = (dt eq size(undef, /type)) && (undef ne 1e-34) ; true if undef is defined
  nan = dt eq 4 ? !values.f_nan : !values.d_nan
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Correlation'

  openw, unit, outname, /get_lun

  outdata = make_array(ns, nl, type = dt)
  
  ix_all = lonarr(nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit
      free_lun, unit  ; close output file
      return
    endif
  
    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = pos, /bil)
    if ignore_undef then begin
      ix = where(data eq undef, cnt)
      if cnt gt 0 then data[ix] = nan
    endif
    for c = 0, ns - 1 do begin
      if ignore_NAN then begin
        ix = where(~finite(data[c, *], /nan), cnt_nan)
      endif else ix = ix_all
      outdata[c, l] = correlate(data[c, ix], prof_data[ix])
    endfor
  endfor
  
  writeu, unit, outdata
  
  close, unit
  free_lun, unit  ; close output file
  
  meta = envi_set_inheritance(fid, dims, /full)
  
  envi_setup_head, fname = outname $
          , data_type = dt $   ; as input
          , /write $
          , interleave = 0 $  ; BSQ
          , nb = 1, nl = nl, ns = ns $
          , bnames = 'Pearson correlation' $
          , inherit = meta

end
