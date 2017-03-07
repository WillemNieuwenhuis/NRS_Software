pro nrs_stack_percentiles, image, outname = outname, exact = exact $
                       , percentile = percentile $
                       , ignore_value = ignore_value $
                       , cancelled = cancelled, prog_obj = prog_obj
  compile_opt idl2, logical_predicate

  cancelled = 1

  if n_elements(percentile) gt 0 then begin
    percentile = fix(strsplit(percentile, ',', /extract))
    ix = where((percentile gt 0) and (percentile lt 100), cix)
    if cix eq 0 then begin
      void = error_message('Percentiles should be between 0% and 100%', title = 'Stack percentiles', /error, /noname, traceback = 0)
      return
    endif
    percentile = percentile[ix]
  endif $
  else percentile = 50  ; median
  percentile = percentile[sort(percentile)] ; percentile are in ascending order
  bnames = string(percentile, format = '(i0)')
  percentile /= 100.0

  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Stack percentiles'

  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt
  inherit = envi_set_inheritance(fid, dims, /full)
  
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_per', ext = '.dat')

  hasIgnore = n_elements(ignore_value) gt 0
  if hasIgnore then ignore_value = (fix(ignore_value, type = dt, /print))[0]

  perc_count = n_elements(percentile)
  outdata = fltarr(ns, perc_count, /nozero)
  openw, out_unit, outname, /get_lun
  
  pperc = rebin(transpose(percentile), ns, perc_count)
  py = rebin(transpose(fix(nb * percentile)), ns, perc_count)
  px = rebin(indgen(ns), ns, perc_count)
  pai = px + py * ns
  
  ivx = []  ; data locations where ignore_data values have been found
  cnt_val = intarr(ns, nb)
  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l + 1, nl, cancelled = cancelled, /console) then return

    data = float(envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil))
    if hasIgnore then begin
      ivx = where(data eq ignore_value, ivx_cnt)
      if ivx_cnt gt 0 then data[ivx] = !values.f_nan
    endif

    ; calculate percentile indices into the data    
    valids = where(finite(data))
    cnt_val[*] = 0
    cnt_val[valids] = 1
    agg_val = total(cnt_val, 2) ; determine valid band values per pixel
    nb_mat = rebin(agg_val, ns, perc_count)
    py = long(nb_mat * pperc)  ; calc the index of the percentile based on actual non-nan values, per pixel
    pai = px + py * ns

    ; sort the data per pixel, NAN values are stored at the end of the data
    ix = sort(data)
    h = histogram(ix mod ns, reverse_indices = ri)  ; order all indices per column 
    six = transpose(reform(ix[ri[ns + 1L : *]], nb, ns))  ; indices are now arranged as BIP
    sorted = data[six]  ; data is now ordered as BIP. The stack values are now sorted per pixel

    outdata[*] = sorted[pai]  ; select the percentile value per pixel for all percentiles
    
    writeu, out_unit, outdata ; write it as BIL
  endfor
  
  envi_setup_head, fname = outname $
        , data_type = size(outdata, /type) $
        , ns = ns, nl = nl, nb = n_elements(percentile) $
        , interleave = 1 $  ; BIL
        , bnames = bnames $
        , /write $
        , inherit = inherit
  
  close, out_unit
  free_lun, out_unit
end
