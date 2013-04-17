pro nrs_stack_quartiles, image, outname = outname, cancelled = cancelled, prog_obj = prog_obj, exact = exact
  compile_opt idl2, logical_predicate

  cancelled = 1
  
  envi_open_file, image, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then return
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Stack quartiles'

  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, data_type = dt
  inherit = envi_set_inheritance(fid, dims, /full)
  
  if (n_elements(outname) eq 0) or (strlen(outname) eq 0) then $
    outname = getoutname(image, postfix = '_quar', ext = '.dat')

  outdata = make_array(ns, 3, type = dt, /nozero)
  openw, out_unit, outname, /get_lun
  
  avg_q13 = 0
  avg_q2 = 0
  if ~keyword_set(exact) then begin
    if nb mod 2 eq 0 then avg_q2 = 1
    if nb mod 4 eq 0 then avg_q13 = 1
  endif
  q1 = nb / 4
  q2 = nb / 2
  q3 = 3 * nb / 4
  for l = 0, nl - 1 do begin
    if (l + 1) mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return

    data = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, /bil)
    
    ix = sort(data)
    h = histogram(ix mod ns, reverse_indices = ri)
    six = transpose(reform(ix[ri[ns + 1L : *]], nb, ns))
    sorted = data[six]
    
    if avg_q13 eq 0 then begin
      outdata[*, 0] = sorted[*, q1]
      outdata[*, 2] = sorted[*, q3]
    endif else begin
      outdata[*, 0] = mean(sorted[*, q1-1:q1], dim = 2)
      outdata[*, 2] = mean(sorted[*, q3-1:q3], dim = 2)
    endelse
    if avg_q2 eq 0 then begin
      outdata[*, 1] = sorted[*, q2]
    endif else begin
      outdata[*, 1] = mean(sorted[*, q2-1:q2], dim = 2)
    endelse
    writeu, out_unit, outdata
  endfor
  
  envi_setup_head, fname = outname $
        , data_type = dt $
        , ns = ns, nl = nl, nb = 3 $
        , interleave = 1 $  ; BIL
        , bnames = ['Q1', 'Median', 'Q3'] $
        , /write $
        , inherit = inherit
  
  close, out_unit
  free_lun, out_unit
end
