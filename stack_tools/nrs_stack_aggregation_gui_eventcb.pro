pro nrs_stack_aggregation_handleGo, Event
  compile_opt idl2, logical_predicate

  list_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_list')
  widget_control, list_fld, get_uvalue = li

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_aggr_combo')
  aggr_meth = widget_info(val_fld, /combobox_gettext)
  
  plist = li.items
  if ~ptr_valid(plist) then return
  
  items = *plist
  nr_files = n_elements(items)
  fids = lonarr(nr_files)
  for f = 0, nr_files - 1 do begin
    envi_open_file, items[f], r_fid = fid, /no_realize, /no_interactive_query
    fids[f] = fid
  endfor
  
  envi_file_query, fids[0], nb = nb, nl = nl, ns = ns $
                 , dims = dims, data_type = dt $
                 , bnames = bnames $
                 , data_ignore_value = ignore $
                 , fname = fname
  mi = envi_get_map_info(fid = fids[0], undef = undef)
  if undef eq 1 then void = temporary(mi)
  
  ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15 $
                        , title = 'Stack: bandwise aggregation')
  progressBar -> Start

  outname = getoutname(fname, postfix = '_' + aggr_meth, ext = '.dat')

  cube = fltarr(ns, nl, nr_files)
  
  openw, unit, outname, /get_lun
  outdata = assoc(unit, fltarr(ns, nl))  ; bsq

  catch, stat
  if stat ne 0 then begin
    nrs_assoc_cleanup, unit, outname, progressBar
    cancelled = 1
    return
  endif

  data = fltarr(ns, nl)
  for b = 0, nb - 1 do begin
    for f = 0, nr_files - 1 do begin
      if nrs_update_progress(progressBar, b * nr_files + f, nr_files * nb) then return

      cube[*, *, f] = envi_get_data(fid = fids[f], dims = dims, pos = [b])
    endfor
    ix = where(cube eq ignore, ig_count)
    if ig_count gt 0 then cube[ix] = !values.f_nan
    
    case aggr_meth of
      'Sum'    : data[*,*] = total(cube, 3, /nan)
      'Mean'   : data[*,*] = total(cube, 3, /nan) / nr_files
      'Min'    : data[*,*] = min(cube, dimension = 3, /nan)
      'Max'    : data[*,*] = max(cube, dimension = 3, /nan)
      'Median' : begin
                   if ig_count gt 0 then begin
                     ; trick to handle NAN values in median
                     mn = min(cube, max = mx, /nan)
                     semi = ig_count / 2
                     cube[ix[0 : min([0, semi - 1])]] = mn
                     cube[ix[semi : ig_count - 1]] = mx
                   endif
                   
                   data[*,*] = median(cube, dimension = 3)
                 end
    endcase
    
    outdata[b] = data
  endfor
  
  close, unit
  free_lun, unit  ; close assoc
  
  envi_setup_head, fname = outname $
          , data_type = size(data, /type) $   
          , /write $
          , interleave = 0 $  ; BSQ
          , nb = nb, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , data_ignore_value = ignore

  for f = 0, nr_files - 1 do begin
    envi_file_mng, id = fids[f], /remove
  endfor
  
  if n_elements(progressBar) gt 0 then $
    progressBar -> Destroy
end

pro nrs_stack_aggregation_listev, event
  if (Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' ) then begin
    if event.clicks eq 1 then $
      if event.index ne -1 then begin
        val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_remove')
        widget_control, val_fld, sensitive = 1
      endif
  endif
end

pro nrs_stack_aggregation_add, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_add')
  widget_control, val_fld, sensitive = 0
  
  list_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_list')
  widget_control, list_fld, get_uvalue = li

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_inputstack')
  widget_control, val_fld, get_value = _inputStack
  inputStack = strtrim(_inputStack)
  envi_open_file, inputStack, /no_realize, /no_interactive_query, r_fid = fin
  envi_file_query, fin, nb = nb, nl = nl, ns = ns
  
  if li.nb eq -1 then begin
    li.nb = nb
    li.nl = nl
    li.ns = ns
  endif else begin
    if (li.nb ne nb) or (li.ns ne ns) or (li.nl ne nl) then begin
      void = error_message('Dimensions of stack do not match', title = 'Stack aggregation error', /error, /noname, traceback = 0)
      return
    endif
  endelse
  
  items = []
  plist = li.items
  if ptr_valid(plist) then begin
    items = *plist
    ptr_free, plist
  endif
  
  pcount = 0
  if n_elements(items) gt 0 then $
    px = where(items eq inputStack, pcount)
  if pcount eq 0 then begin
    if n_elements(items) eq 0 then items = [inputStack] $
    else items = [items, inputStack]
    widget_control, list_fld, set_value = items
  endif
  
  li.items = ptr_new(items, /no_copy)
  widget_control, list_fld, set_uvalue = li
end

pro nrs_stack_aggregation_remove, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_remove')
  widget_control, val_fld, sensitive = 0
  
  list_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_list')
  sel = widget_info(list_fld, /list_select)
  if sel eq -1 then return
  
  widget_control, list_fld, get_uvalue = li

  plist = li.items
  if ptr_valid(plist) then begin
    items = *plist
    ptr_free, plist
  endif
  item_cnt = widget_info(list_fld, /list_number)
  if item_cnt gt 1 then begin
    ix = indgen(item_cnt - 1)
    if sel lt item_cnt - 1 then $
      ix[sel: item_cnt - 2] += 1
      
    items = items[ix]
    widget_control, list_fld, set_value = items
  endif else begin
    void = temporary(items) 
    widget_control, list_fld, set_value = ['']
  endelse

  if n_elements(items) eq 0 then li.nb = -1
  li.items = ptr_new(items, /no_copy)
  widget_control, list_fld, set_uvalue = li
  
end

pro nrs_stack_aggregation_handle_inputfile, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_inputstack')
  widget_control, val_fld, get_value = inputStack

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_aggregation_add')
  widget_control, val_fld, sensitive = (strlen(strtrim(inputStack)) gt 0)
  
end

