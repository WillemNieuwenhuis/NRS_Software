pro nrs_rainfall_consecutive, inname, dry = dry, wet = wet $
           , outname = outname $
           , dry_limit = dry_limit, high_limit = high_limit $
           , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 1

  wet = keyword_set(wet)
  dry = ~wet && keyword_set(dry)
  wet = wet || ~dry
  
  if n_elements(dry_limit) gt 0 then dry_limit = float(dry_limit)
  if n_elements(high_limit) gt 0 then high_limit = float(high_limit)
  
  envi_open_file, inname, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  envi_query_file, fid, ns = ns, nl = nl, nb = nb, dims = dims
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nrs_set_progress_property, prog_obj, /start, title = 'Precipitation indices'

  if n_elements(outname) eq 0 && strlen(strtrim(outname, 2)) eq 0 then begin
    outname = getOutname(inname, postfix = '_prix', ext = '.dat')
  endif

  out_data = fltarr(ns, nl, 2)

  nbw = nb + 2
  work = fltarr(ns, nbw)
  mul = rebin(indgen(ns) + 1, ns, nb)
  pos = indgen(nb)
  for line = 0, nl - 1 do begin
    slice = envi_get_slice(fid, line = line, xs = 0, xe = ns - 1, pos = pos, /bil)
    
    dry = slice lt dry_limit
    dry = dry * mul
    work[*, 1 : nbw - 1] = dry
    work = reform(work, ns * nbw, /overwrite)
    ix = where(work[1:*] ne work, cnt)
    if cnt gt 0 then begin
      ix = reform(ix, 2, cnt / 2, /overwrite)
      diff = ix[1, *] - ix[0, *]
      mx = fltarr(nb, ns)
      mx[ix[0, *]] = diff
      out_data[*, line, 0] = max(mx, dim = 2)
    endif
    work = reform(work, ns, nbw, /overwrite)

;    wet = slice ge dry_limit
;    wet = wet * mul
;    wet = reform(wet, ns * nb, /overwrite)
;    wetlab = label_region(wet)
;    hwet = histogram(wetlab, min = 1, binsize = 1, rev = ri)
  endfor
  
end

pro work_test
  compile_opt idl2, hidden
  
  slice = [[0,0,0,1],[1,0,1,1],[1,0,0,1],[1,1,0,1],[0,1,0,0]]
  print, working(slice)

  slice = 1 - slice
  print, working(slice)
  
  slice = randomu(13,15,17)
  print, working(slice)
end

function working, inslice
  compile_opt idl2, hidden
  
  slice = transpose(inslice)
  
  dry_limit = 0.8
  ns = (size(slice, /dim))[0]
  nb = (size(slice, /dim))[1]
  nsw = ns + 2
  
  mul = rebin(transpose(indgen(nb) + 1), ns, nb)
  
  work = fltarr(nsw, nb)

  dry = slice lt dry_limit
  work[1 : nsw - 2, *] = dry * mul
  work = reform(work, nsw * nb, /overwrite)
  ix = where(work[1:*] ne work, cnt) + 1
  if cnt gt 0 then begin
    ix = reform(ix, 2, cnt / 2, /overwrite)
    diff = ix[1, *] - ix[0, *]
    mx = fltarr(nsw, nb)
    mx[ix[0, *]] = diff
    out_data = fix(max(mx, dim = 1))
    return, out_data
  endif
  return,[]
end  