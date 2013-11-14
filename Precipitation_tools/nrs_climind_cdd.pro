pro nrs_climind_cdd_count, data_in, data_ndx, ns, line, nb, out_data = out_data
  compile_opt idl2, logical_predicate
  
  for s = 0, ns - 1 do begin
    cnt_l = 0
    i = 1
    max_l = -1
    while i lt nb do begin
      if data_in[s, i] eq 1 then ++cnt_l else begin
        max_l = max([cnt_l, max_l])
        cnt_l = 0
      endelse 
      ++i
    endwhile
    out_data[s, line, data_ndx] = max_l
  endfor
end

;+
; :description:
;    Calculate the longest period of a dry spell or a wet period. The criteria for
;    wet or dry is determined by the dry_limit: dry = precipitation < dry_limit;
;    wet = precipitation >= dry_limit
;    At least one of the calcdry, calcwet and calchighwet need to be set; if 
;    not calcwet is assumed.
;
; :params:
;    inname : in, required
;      Name of the input time series
;
; :keywords:
;    calcdry : in
;      If specified and set let the software calculate the longest dry spell
;    calcwet : in
;      If specified and set let the software calculate the longest wet spell
;    calchighwet : in
;      If specified and set let the software calculate the longest very wet spell,
;      consequtive days with precipitation higher than high_limit
;    outname : in
;      The name of the output
;    dry_limit : in, default 1 (mm)
;      The limit dividing dry and wet
;    high_limit : in, default 20 (mm)
;      the limit above which precipitation is considered very heavy
;    prog_obj : in, optional
;      Progressbar object to indicate progress of the calculation
;    cancelled : in, optional
;      If a progressbar is used, signals that the user stopped the calculation
;
; :author: nieuwenhuis
;-
pro nrs_climind_cdd, inname, calcdry = dry, calcwet = wet, calchighwet = high_wet $
           , outname = outname $
           , dry_limit = dry_limit, high_limit = high_limit $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  do_wet = keyword_set(wet)
  do_dry = keyword_set(dry)
  do_high = keyword_set(high_wet)
  do_wet = do_wet || ~(do_wet || do_dry || do_high)
  
  if n_elements(dry_limit) gt 0 then dry_limit = float(dry_limit) else dry_limit = 1
  if n_elements(high_limit) gt 0 then high_limit = float(high_limit) else high_limit = 20
  
  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_file_query, fid, ns = ns, nl = nl, nb = nb, dims = dims
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nrs_set_progress_property, prog_obj, /start, title = 'Climate indices'

  if (n_elements(outname) eq 0) || (strlen(strtrim(outname, 2)) eq 0) then begin
    outname = getOutname(inname, postfix = '_prix', ext = '.dat')
  endif

  t1 = systime(1)
  index = 0
  bnames = []
  if do_dry then begin
    dry_ndx = index++
    bnames = [bnames, 'CDD']
  endif
  if do_wet then begin
    wet_ndx = index++
    bnames = [bnames, 'CWD']
  endif
  if do_high then begin
    high_ndx = index++
    bnames = [bnames, string(high_limit, format = '("CVWD",i0,"mm")')]
  endif
    
  out_data = fltarr(ns, nl, index)

  pos = indgen(nb)
  for line = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, line, nl, cancelled = cancelled) then return
    
    slice = envi_get_slice(fid = fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    if do_dry then begin
      dry = slice lt dry_limit
      nrs_climind_cdd_count, dry, dry_ndx, ns, line, nb, out_data = out_data
    endif

    if do_wet then begin
      wet = slice ge dry_limit
      nrs_climind_cdd_count, wet, wet_ndx, ns, line, nb, out_data = out_data
    endif
    
    if do_high then begin
      high = slice ge high_limit
      nrs_climind_cdd_count, high, high_ndx, ns, line, nb, out_data = out_data
    endif
    
    if line eq 0 then begin
      t2 = systime(1)
      print,'estimated time left: ' + nrs_sec_to_string((t2-t1) * nl)
    endif 
  endfor
  
  inherit = envi_set_inheritance(fid, dims, /full)
  envi_write_envi_file, out_data, out_name = outname, inherit = inherit, bnames = bnames
  print, 'Total running time: ' + nrs_sec_to_string((systime(1)-t1))
  
end

