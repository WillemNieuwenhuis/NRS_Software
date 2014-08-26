;+
; :description:
;    Extract NDVI values from the input stack and copy them to the output stack,
;    when in the growing season; the growing season is available for each time period per class   
;
; :params:
;    image : in, required
;      The input timeseries
;    classfile : in, required
;      The class image
;    season_table : in, required
;      The table indicating the growing season for each class (for one year)
;
; :keywords:
;    img_per_year : in
;      The number of images per year; if not specified it will be determined from the number of table columns 
;    outname : in
;      Name of the output timeseries; if not specified the name of the input timeseries will be used as template
;    start_date : in
;      The start date (dd-mm-yyy) of the input timeseries
;    end_date : in
;      The end date (dd-mm-yyy) of the input timeseries
;    prog_obj : in, optional
;      A progress indicator object (ProgressBar class)
;    cancelled : out
;      When set, indicates an error, or user abort
;
; :author: nieuwenhuis
;-
pro nrs_apply_season_filter, image, classfile, season_table $
       , img_per_year = img_per_year $
       , outname = outname $
       , start_date = start_date $
       , end_date = end_date $
       , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  
  if n_params() ne 3 then begin
    ans = dialog_message('Not enough parameters to continue', title = 'Error', /error)
    return
  endif
  
  envi_open_file, image, r_fid = fid, /no_interactive_query, /no_realize
  if fid eq -1 then  begin
    ans = dialog_message('Could not open timeseries', title = 'Error', /error)
    return
  endif
  
  sd = nrs_str2julian(start_date)
  ed = nrs_str2julian(end_date)
  if ed lt sd then begin
    ans = dialog_message('End date before start date', title = 'Error', /error)
    return
  endif

  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns $
                 , data_type = dt, bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = crd_undef)
  if crd_undef eq 1 then mi = temporary(dummy)
  
  envi_open_file, classfile, r_fid = class, /no_realize, /no_interactive_query
  if class eq -1 then  begin
    ans = dialog_message('Could not open class map', title = 'Error', /error)
    return
  endif

  season = nrs_read_csv(season_table, header = header, record_start = 2)
  field_count = n_tags(season)
  if n_elements(img_per_year) eq 0 then img_per_year = field_count - 1 ; don't count the start column header
  
  cancelled = 0
  
  nrs_load_class_image, class, cldata = cldata, cnames = cnames, num_classes = nrclass $
                      , has_unclassified = has_unclassified $
                      , class_adjust = 0
  
  caldat, sd, mm, dd, yy
  sdy = julday(1,1,yy)
  nrs_get_dt_indices, [sd, ed], julian_out = jo, period = '10-day'
  nrs_get_dt_indices, [sdy, ed], julian_out = joy, period = '10-day'
  offset = where(jo[0] eq joy, cnt)
  
  classes = season.(0)
  tbl_data = intarr(n_elements(season.(0)), field_count - 1)
  for c = 1, field_count - 1 do begin
    tbl_data[*, c - 1] = season.(c)
  endfor

  nrs_set_progress_property, prog_obj, /start, title = 'Create filter mask'
  
  ; create mask bands
  mask = bytarr(ns * nl, img_per_year)
  for cli = 0, n_elements(classes) - 1 do begin
    cl = classes[cli]
    ix = where(cldata eq cl, cnt)
    if cnt gt 0 then begin
      mask[ix, *] = rebin(tbl_data[cli, *], cnt, img_per_year)
    endif
  endfor
  mask = reform(mask, ns, nl, img_per_year, /overwrite)
  
  nrs_set_progress_property, prog_obj, title = 'Applying growing season filter'
  
  ; mask the actual data one band at a time
  if n_elements(outname) eq 0 then outname = getoutname(image, postfix = '_mask', ext = '.dat')
  openw, unit, outname, /get_lun
  
  ; first determine year band index
  bind = (indgen(nb) + offset[0]) mod img_per_year
  
  for band = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, band, nb, cancelled = cancelled) then begin
      close, unit
      free_lun, unit
    endif
    data = envi_get_data(fid = fid, dims = dims, pos = band)
    data[*] *= mask[*, *, bind[band]]
    writeu, unit, data
  endfor
    
  meta = envi_set_inheritance(fid, dims, /full)
  
  envi_setup_head, fname = outname $
    , data_type = dt $   ; same as input
    , /write $
    , interleave = 0 $  ; BSQ
    , nb = nb, nl = nl, ns = ns $
    , bnames = bnames $
    , inherit = meta $
    , data_ignore_value = undef
    

  close, unit
  free_lun, unit
  
end

