;+
; :description:
;    Aggregate time series data (monthly) in 2 breeding seasons per year. 
;    Resuls in average value for both breeding seasons per year.
;    Time series should contain complete years.
;    If a breeding season crosses a year boundary it is taken to belong to the year
;    of the start of the season. The breeding seasons are indicated in the groups array.
;    Each group is a sub array with the months belonging to the breeding period.
;    Month numbers start at 1 (for January).
;    
;    Example:
;      nrs_select_aggregate,'timeseries.dat',start_date='1-1-2005',end_date='31-12-2009',groups=[[3,4,5],[1,2,12]]
;
; :params:
;    image  : in
;      The name of the time series with monthly data (with full path)
;
; :keywords:
;    start_date : in, required
;      The start date of the time series
;    end_date : in, required
;      The end date of the time series
;    groups : in, required
;      2-D array with the two groups
;    prog_obj : in, optional
;      Progress indicator object
;    cancelled : out
;      If set indicates failure or user abort 
;
; :Author: nieuwenhuis
; 
; :history:
;   - nov 2015: created
;-
pro nrs_select_aggregate, image $
                        , start_date = start_date, end_date = end_date $
                        , groups = groups $
                        , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1

  if size(groups, /n_dim) ne 2 then begin
    ans = dialog_message('Incorrect number of aggregation groups', title = 'Error', /error)
    return
  endif
  
  groups = fix(groups) - 1  ; turn one-based into zero-based
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then  begin
    ans = dialog_message('Could not open timeseries', title = 'Error', /error)
    return
  endif

  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns $
    , data_type = dt, bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = crd_undef)
  if crd_undef eq 1 then mi = temporary(dummy)

  meta = envi_set_inheritance(fid, dims, /full)

  cancelled = 0
  
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    tmp = ed
    ed = sd
    sd = temporary(tmp)
  endif
  caldat, sd, mm, dd, start_yy
  caldat, ed, mm, dd, end_yy
  sdy = julday(1, 1, start_yy)
  day_per = nrs_get_period_from_range(sd, ed, nb, per_str = ip)
  per_per_year = 12 ; monthly data
  nr_years = nb / 12

  ; handle nodata values
  if n_elements(undef) eq 0 then undef = 0
;  if (size(undef, /type) eq 5) and (undef eq 1e-34) then undef = 255

  nrs_set_progress_property, prog_obj, /start, title = 'Calculate breeding averages'

  ; calculate band indices for each breeding group
  mult = transpose(indgen(nr_years) * 12)
  gr_cur = groups[*, 0]
  off = where((shift(gr_cur, -1) - gr_cur) gt 1, off_cnt)
  gr_elem1 = n_elements(gr_cur)
  mult1 = rebin(mult, gr_elem1, nr_years)
  gr_ix1 = rebin(gr_cur, n_elements(gr_cur), nr_years) + mult1
  gr_ix1 = reform(gr_ix1, n_elements(gr_ix1), /overwrite)
  if off_cnt gt 0 then begin
    nr_dup = gr_elem1 - off
    gr_ix1 = [gr_ix1[nr_dup : -1], intarr(nr_dup) + gr_ix1[-1]]
  endif

  gr_cur = groups[*, 1]
  off = where((shift(gr_cur, -1) - gr_cur) gt 1, off_cnt)
  gr_elem2 = n_elements(gr_cur)
  mult2 = rebin(mult, gr_elem2, nr_years)
  gr_ix2 = rebin(gr_cur, n_elements(gr_cur), nr_years) + mult2
  gr_ix2 = reform(gr_ix2, n_elements(gr_ix2), /overwrite)
  if off_cnt gt 0 then begin
    nr_dup = off + 1
    gr_ix2 = [gr_ix2[nr_dup : -1], intarr(nr_dup) + gr_ix2[-1]]
  endif

  ; open the output files
  pf1 = string(groups[0, 0] + 1, format = '(i0)')
  pf2 = string(groups[0, 1] + 1, format = '(i0)')
  outname1 = getoutname(image, postfix = '_brd_' + pf1, ext = '.dat')  
  outname2 = getoutname(image, postfix = '_brd_' + pf2, ext = '.dat')
  outnamecv1 = getoutname(image, postfix = '_brd_cv' + pf1, ext = '.dat')
  outnamecv2 = getoutname(image, postfix = '_brd_cv' + pf2, ext = '.dat')
  openw, unit1, outname1, /get_lun
  openw, unit2, outname2, /get_lun
  openw, unit3, outnamecv1, /get_lun
  openw, unit4, outnamecv2, /get_lun

  for l = 0, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then begin
      close, unit1
      free_lun, unit1
      close, unit2
      free_lun, unit2
      close, unit3
      free_lun, unit3
      close, unit4
      free_lun, unit4
      return
    endif

    slice1 = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = gr_ix1)
    slice2 = envi_get_slice(fid = fid, line = l, xs = 0, xe = ns - 1, pos = gr_ix2)
    slice1 = reform(slice1, ns, gr_elem1, nr_years, /over)
    slice2 = reform(slice2, ns, gr_elem2, nr_years, /over)
    data_out1 = mean(slice1, dim = 2)   ; min, max, total, mean, stddev; moment
    data_out2 = mean(slice2, dim = 2)
    data_cv1 = stddev(slice1, dim = 2) / data_out1
    data_cv2 = stddev(slice2, dim = 2) / data_out2
    
    writeu, unit1, float(data_out1)
    writeu, unit2, float(data_out2)
    writeu, unit3, float(data_cv1)
    writeu, unit4, float(data_cv2)
  endfor
  close, unit1
  free_lun, unit1
  close, unit2
  free_lun, unit2
  close, unit3
  free_lun, unit3
  close, unit4
  free_lun, unit4
  
  form = '("Year: ",i04)'
  bnames = string(indgen(nr_years) + start_yy, format = form)
  envi_setup_head, fname = outname1 $
        , data_type = 4 $   ; 4 == float
        , /write $
        , interleave = 1 $  ; 1 = BIL
        , nb = nr_years, nl = nl, ns = ns $
        , bnames = bnames $
        , data_ignore_value = nodata_out $
        , inherit = meta

  envi_setup_head, fname = outname2 $
        , data_type = 4 $   ; 4 == float
        , /write $
        , interleave = 1 $  ; 1 = BIL
        , nb = nr_years, nl = nl, ns = ns $
        , bnames = bnames $
        , data_ignore_value = nodata_out $
        , inherit = meta

  envi_setup_head, fname = outnamecv1 $
        , data_type = 4 $   ; 4 == float
        , /write $
        , interleave = 1 $  ; 1 = BIL
        , nb = nr_years, nl = nl, ns = ns $
        , bnames = bnames $
        , data_ignore_value = nodata_out $
        , inherit = meta

  envi_setup_head, fname = outnamecv2 $
        , data_type = 4 $   ; 4 == float
        , /write $
        , interleave = 1 $  ; 1 = BIL
        , nb = nr_years, nl = nl, ns = ns $
        , bnames = bnames $
        , data_ignore_value = nodata_out $
        , inherit = meta
end
