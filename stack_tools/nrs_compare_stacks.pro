function nrs_compare_stack_msg, msg, v1, v2, t1, t2
  p1 = t1 + '=' + string(v1, format = '(i0)')
  p2 = t2 + '=' + string(v2, format = '(i0)')
  return, msg + ': ' + p1 + ', ' + p2
end
;
;+
; :Description:
;    Compare two stacked images. This routines checks whether two stacks are equal, 
;    and if not reports where the stacks differ. This is done on different levels:
;    <ol>
;    <li>Check number of samples, lines and bands
;    <li>Check number of band names
;    <li>Check coordinate systems
;    <li>If the number of samples and lines are equal, compare all the band values;
;    If the number of bands are not equal only the first bands are checked upto the
;    minumum of the number of bands of both stacks. 
;    </ol>
;
; :Params:
;    st1 : in
;      The first stacked image
;    st2 : in
;      The second stacked image
;
; :Keywords:
;    show_bnames : in
;      If set, reports the list of band numbers where the band names are different
;    show_bands : in
;      If set, reports the list of band numbers where the band values are different
;    report_file : in
;      Text file, reporting all the differences; if not specified, the differences
;      are reported in the console
;    prog_obj : in
;      A progress indicator object. If specified causes the progress to be displayed
;    cancelled : out
;      Will be set if the progress is interrupted by the user 
;
; :Author: nieuwenhuis
;-
pro nrs_compare_stacks, st1, st2, show_bnames = show_bnames, show_bands = show_bands $
                      , report_file = report_file $
                      , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, st1, r_fid = fid1, /no_realize, /no_interactive_query
  if fid1 eq -1 then return
  
  envi_open_file, st2, r_fid = fid2, /no_realize, /no_interactive_query
  if fid2 eq -1 then return
  
  cancelled = 0
  envi_file_query, fid1, ns = ns1, nl = nl1, nb = nb1, dims = dims1, bnames = bnames1
  mi1 = envi_get_map_info(fid = fid1, undefined = undef1)
  
  envi_file_query, fid2, ns = ns2, nl = nl2, nb = nb2, dims = dims2, bnames = bnames2
  mi2 = envi_get_map_info(fid = fid2, undefined = undef2)
  
  diffs = []
  if ns1 ne ns2 then diffs = [diffs, nrs_compare_stack_msg('Number of columns differ', ns1, ns2, 'source', 'target')]
  if nl1 ne nl2 then diffs = [diffs, nrs_compare_stack_msg('Number of lines differ', nl1, nl2, 'source', 'target')]
  if nb1 ne nb2 then diffs = [diffs, nrs_compare_stack_msg('Number of bands differ', nb1, nb2, 'source', 'target')]
   
  if n_elements(bnames1) ne n_elements(bnames2) then diffs = [diffs, 'Number of band names differ']
  bn_nb = min([n_elements(bnames1), n_elements(bnames2)])
  ix = where(bnames1[0:bn_nb-1] ne bnames2[0:bn_nb-1], count)
  if count gt 0 then begin
    diffs = [diffs, 'Band names differ']
    if ~(n_elements(show_bnames) eq 0 || show_bnames eq 0) then begin
      fmt = '(i0,' + string(bn_nb - 1, format = '(i0)') + '(",",i0))' 
      bands = string(ix, format = fmt)
      diffs = [diffs, 'Bandnrs:' + bands]
    endif
  endif
  
  if undef1 ne undef2 then diffs = [diffs, 'One of the CSYs is undefined']
  
  if ns1 eq ns2 && nl1 eq nl2 then begin
    nrpix = ns1 * nl1
    nb = min([nb1, nb2])
    b_eq = bytarr(nb)
    for b = 0, nb - 1 do begin
      d1 = envi_get_data(fid = fid1, dims = dims1, pos = b)
      d2 = envi_get_data(fid = fid2, dims = dims2, pos = b)
      d1_nan = where(finite(d1, /nan), c_nan1)
      d2_nan = where(finite(d2, /nan), c_nan2)
      eq_nan = 0
      if (c_nan1 gt 0) && (c_nan1 eq c_nan2) then eq_nan = total(d1_nan eq d2_nan)
      b_eq[b] = total(d1 eq d2) + eq_nan eq nrpix 
    endfor
    
    ix = where(b_eq eq 0, count)
    if count gt 0 then begin
      diffs = [diffs, 'Band values differ']
      if ~(n_elements(show_bands) eq 0 || show_bands eq 0) then begin
        fmt = '(i0,' + string(count - 1, format = '(i0)') + '(",",i0))' 
        bands = string(ix, format = fmt)
        diffs = [diffs, 'Bandnrs:' + bands]
      endif
    endif
  endif
  
  rep_msg = ', details in console.'
  rep_out = (n_elements(report_file) gt 0) and (strlen(strtrim(report_file, 2)) gt 0)
  if  rep_out then rep_msg = ', see ''' + report_file + ' ''for details.'
  if n_elements(diffs) eq 0 then begin
    ans = dialog_message('Stacks are equal', /information)
  endif else begin
    if rep_out then begin
      openw, lun, report_file, /get_lun
      printf, lun, diffs, format = '(a)'
      close, lun
    endif else begin
      print, diffs, format = '(a)'
    endelse
    ans = dialog_message('Stacks are different' + rep_msg, /information)
  endelse
end