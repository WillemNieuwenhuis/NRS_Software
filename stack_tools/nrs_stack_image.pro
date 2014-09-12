;+
; :Description:
;    Stack a series of images and 'stack' them into a single file.
;    It retains the coordinate system.
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
;      Can be either a single filename or an array of filenames
;      
;      In case of a single filename it points to a text file containing the list
;      with all filenames of the files to stack.
;      
;      In case of a file list, it is treated as the actual list of files to stack
;    extension : in, optional, default = '*' (all files)
;      Define the allowed data file extension
;    prog_obj : in, optional
;      A ProgressBar object to indicate progress
;    cancelled : out, optional
;      If set, the user stopped the process
;
; :Author: nieuwenhuis
;-
pro nrs_stack_image, outname, folder = folder, list_file = list_file $
                   , extension = extension $
                   , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  nrs_set_progress_property, prog_obj, title = 'Stacking layers', /start
  
  if n_elements(extension) eq 0 then extension = '*'
  nrfiles = n_elements(list_file)
  doFolder = n_elements(folder) eq 1
  doFolder = doFolder && (strlen(strtrim(folder, 2)) gt 0)
  useList = (~doFolder) && (nrfiles ge 1)
  if useList eq 1 then begin
    if nrfiles gt 1 then lst = list_file $
    else begin
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
    endelse
  endif
  if doFolder eq 1 then begin
    lst = nrs_find_images(folder, '.*', extension = '*', /exclude_hdr)
    if n_elements(lst) eq 0 then begin
      void = error_message('No files found', traceback = 0, /error)
      return
    endif
    lst = lst[sort(lst)]
    lst = nrs_get_envi_datafilename(lst)
  endif
  
  cancelled = 0

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
    envi_file_query, fid, nb = nb, nl = nl, ns = ns, dims = dims $
                        , bnames = l_bnames, data_ignore_value = undef_loc

    if (n_elements(undef_loc) gt 0) && (n_elements(undef) eq 0) then undef = undef_loc
     
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
  data = make_array(ns, nl, type = dt)
  bcnt = 0
  
  openw, unit, outname, /get_lun
  
  nrs_set_progress_property, prog_obj, title = 'Stacking layers', /start

  for f = 0, nf - 1 do begin
    if nrs_update_progress(prog_obj, f, nf, cancelled = cancelled) then return

    if incMulti eq 1 then envi_file_query, fids[f], nb = nb $
    else nb = 1
    for b = 0, nb - 1 do begin
      data[*] = envi_get_data(fid = fids[f], dims = dims, pos = b)
      writeu, unit, data
    endfor
    bcnt += nb
    
    envi_file_mng, id = fids[f], /remove
  endfor
  close, unit
  free_lun, unit
  
  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , nb = bcnt $
          , nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , interleave = 0 $
          , data_ignore_value = undef
end