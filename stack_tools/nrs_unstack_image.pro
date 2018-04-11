;+
; :Description:
; Unstack a timeseries / hyperspectral image and 'unstack' it into separate
; files, one for each band; retains the coordinate system
; The output files are created in the same location as the input
; where each file will be postfixed with the sequential band number
; 
; :params:
;   fid : in, required
;     File handle of the timeseries / hyperspectral image
;
; :Keywords:
;    Basename : in, optional
;      Basename of the output files; if not specified the name of input is used as basename 
;    list_file : in/out, optional
;      Text file containing the list with all filenames of the unstacked layers
;    prog_obj : in, optional
;      A ProgressBar object to indicate progress
;    cancelled : out, optional
;      If set, the user stopped the process
;
; :Author: nieuwenhuis
;-
pro nrs_unstack_image, fid, basename = basename, list_file = list_file, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, title = 'Unstacking layers', /start
  
  envi_file_query, fid, nb = nb, nl = nl, ns = ns, dims = dims, fname = imgname, bnames = bnames
  mi = envi_get_map_info(fid = fid, undefined = undefined)
  if n_elements(basename) eq 1 then imgname = basename 

  if n_elements(list_file) eq 0 then list_file = imgname
  filelist = getOutname(list_file, ext = '.txt', postfix = '')
  format = '(i0' + string(alog10(nb) + 1,format="(i0)") + ')'
  openw, lun, filelist, /get_lun
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then begin
      break
    endif
    
    outname = getOutname(imgname, postfix = '_' + string(b, format=format), ext='.dat')
    printf, lun, outname
    data = envi_get_data(fid = fid, dims = dims, pos = b)
    bname = 'band_' + string(b, format = format)
    if n_elements(bnames) gt b then bname = bnames[b]
    envi_write_envi_file, data, out_name = outname, bnames = [bname], /no_copy, /no_open, map_info = mi
  endfor
  close, lun
end

