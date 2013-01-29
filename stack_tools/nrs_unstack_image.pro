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
  openw, lun, filelist, /get_lun
  for b = 0, nb - 1 do begin
    if nrs_update_progress(prog_obj, b, nb, cancelled = cancelled) then return
    
    outname = getOutname(imgname, postfix = '_' + string(b, format='(i03)'), ext='.')
    printf, lun, outname
    data = envi_get_data(fid = fid, dims = dims, pos = b)
    bname = 'band_' + string(b, format='(i03)')
    if n_elements(bnames) gt b then bname = bnames[b]
    envi_write_envi_file, data, out_name = outname, bnames = [bname], /no_copy, /no_open, map_info = mi
  endfor
  close, lun
end

;+
; :Description:
; Stack a series of images and 'stack' them into a single file.
; It retains the coordinate system.
; 
; :params:
;    outname : in, required
;      Output name of the new stack
;
; :Keywords:
;    folder : in, optional
;      Folder that contains all the images to stack; the folder keyword takes precedent
;      over list_file
;    list_file : in, optional
;      Text file containing the list with all filenames of the files to stack
;    prog_obj : in, optional
;      A ProgressBar object to indicate progress
;    cancelled : out, optional
;      If set, the user stopped the process
;
; :Author: nieuwenhuis
;-
pro nrs_stack_image, outname, folder = folder, list_file = list_file, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, title = 'Stacking layers', /start
  
  doFolder = n_elements(folder) eq 1
  doFolder = doFolder and (strlen(strtrim(folder, 2)) gt 0)
  useList = (~doFolder) and (n_elements(list_file) eq 1)
  if useList eq 1 then begin
    if ~(file_info(list_file)).exists then begin
      void = error_message('List file not found', traceback = 0, /error)
      return
    endif
    
    lbl = strarr(file_lines(list_file))
    openr, lun, list_file, /get_lun
    readf, lun, lbl
    close, lun
    free_lun, lun
    lst = strtrim(lbl, 2)
  endif
  if doFolder eq 1 then begin
    lst = nrs_find_images(folder, '.*', extension = 'hdr')
    if n_elements(lst) eq 0 || lst[0] eq -1 then begin
      void = error_message('No files found', traceback = 0, /error)
      return
    endif
    lst = lst[sort(lst)]
  endif
  
  ; check sizes
  ans = -1
  anl = -1
  anb = 0
  incMulti = 0
  askMulti = 0
  dims = -1

  nrs_set_progress_property, prog_obj, title = 'Checking image dimensions', /start

  for fn = 0, n_elements(lst) - 1 do begin
    if nrs_update_progress(prog_obj, fn, n_elements(lst), cancelled = cancelled) then return
    
    envi_open_file, lst[fn], r_fid = fid, /no_realize, /no_interactive_query
    envi_file_query, fid, nb = nb, nl = nl, ns = ns, dims = dims, bnames = l_bnames
    if (~askMulti) and (nb gt 1) then begin
      askMulti = 1
      answer = dialog_message('Also include files with multiple layers?', Title = 'Stacking', /question)
      incMulti = answer eq 'Yes'
    endif
    if askMulti && ~incMulti && (nb gt 1) then continue
    anb += nb
    
    if nb eq 1 then l_bnames = file_basename(getOutname(lst[fn], postfix = '', ext = '.'))
    
    if n_elements(fids) eq 0 then begin
      fids = [fid]
      bnames = [l_bnames]
    endif else begin
      fids = [fids, fid]
      bnames = [bnames, l_bnames]
    endelse
    if ans eq -1 then begin
      ans = ns
      anl = nl
    endif
    if (ans ne ns) or (anl ne nl) then begin
      void = error_message('Not all maps have the same dimensions!', traceback = 0, /error)
      return
    endif
  endfor
  
  if n_elements(fids) eq 0 then begin
    void = error_message('No files found with one layer!', traceback = 0, /error)
    return
  endif
  
  envi_file_query, fids[0], nl = nl, ns = ns, dims = dims, data_type = dt
  mi = envi_get_map_info(fid = fids[0], undefined = undefined)
  if n_elements(basename) eq 1 then imgname = basename 

  nf = n_elements(fids)
  if incMulti eq 1 then nb = anb else nb = nf
  data = make_array(ns, nl, nb, type = dt)
  bcnt = 0
  
  nrs_set_progress_property, prog_obj, title = 'Stacking layers', /start

  for f = 0, nf - 1 do begin
    if nrs_update_progress(prog_obj, f, nf, cancelled = cancelled) then return

    if incMulti eq 1 then envi_file_query, fids[f], nb = nb $
    else nb = 1
    for b = 0, nb - 1 do begin
      data[*, *, bcnt + b] = envi_get_data(fid = fids[f], dims = dims, pos = b)
    endfor
    bcnt += nb
    
    envi_file_mng, id = fids[f], /remove
  endfor

  envi_write_envi_file, data, out_name = outname, bnames = bnames, /no_copy, /no_open, map_info = mi
end