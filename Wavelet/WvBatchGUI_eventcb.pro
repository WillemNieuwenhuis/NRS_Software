pro handleBatchBrowseInput, Event
	widget_control, event.top, get_uvalue=state
  id = widget_info(event.top, find_by_uname='InputListNameText')

  nrs_ask_file, id, 'Specify filename for image list file'
  
	state.coefdirection = 0
	widget_control, event.top, set_uvalue=state
end

pro handleBatchBrowseCoefficient, event
	widget_control, event.top, get_uvalue=state
  id = Widget_Info(event.top, FIND_BY_UNAME='CoefficientBatchText')
  widget_control, id, get_value=folder

	folder = DIALOG_PICKFILE(title='Specify folder for coefficients output', path=folder, /directory)
	if strlen(folder) eq 0 then return

	state.coefdirection = 0
	widget_control, id, set_value=folder
	widget_control, event.top, set_uvalue=state
end

pro handleSplitToggle, Event
  widget_control, event.top, get_uvalue=state
  id = widget_info(event.top, find_by_uname='inputSplitToggle')
  splitFile = widget_info(id, /button_set)
  splitNameId = widget_info(event.top, find_by_uname='inputSplitImageNamePanel')
  splitSizeId = widget_info(event.top, find_by_uname='inputSplitSizePanel')
  listFileId = widget_info(event.top, find_by_uname='inputListNamePanel')
  widget_control, splitNameId, sensitive = splitFile
  widget_control, splitSizeId, sensitive = splitFile
  widget_control, listFileId,  sensitive = 1 - splitFile
end

pro handleBrowseSplitImage, event
  widget_control, event.top, get_uvalue=state
  id = widget_info(event.top, find_by_uname='inputSplitToggle')
  splitFile = widget_info(id, /button_set)
  if splitFile eq 0 then return  ; should be possible, but check anyway
  id = widget_info(event.top, find_by_uname='InputSplitImageNameText')
  
  nrs_ask_file, id, 'Select input image to split and transform'
  
  widget_control, id, get_value = splitImageName
  envi_open_file, splitImageName, r_fid = fid, /no_realize, /no_interactive_query
  if fid ne -1 then begin
    envi_file_query, fid, ns = ns, nl = nl
    id = widget_info(event.top, find_by_uname='inputSplitMaxSizeLabel')
    widget_control, id, set_value = string([ns, nl], format = '("  Input image (X,Y): (", i0,",",i0,")")')
    id = widget_info(event.top, find_by_uname='InputSplitSizeText')
    widget_control, id, get_value = splitSizeString
    on_ioerror, bad_size
      splitSize_arr = fix(splitSizeString)
      splitSize = splitSize_arr[0]
      mx = max([ns, nl])
      if splitSize gt mx then $
        widget_control, id, set_value = string(mx / 2, format = '(i0)')
      
    bad_size:
      on_ioerror, NULL
  endif
end

pro handleBatchCalculate, event
	widget_control, event.top, get_uvalue=state
	id = Widget_Info(event.top, FIND_BY_UNAME='InputListNameText')
	widget_control, id, get_value=filelistName
	id = Widget_Info(event.top, FIND_BY_UNAME='CoefficientBatchText')
  widget_control, id, get_value=folder
  id = widget_info(event.top, find_by_uname='OrderBatchText')
	widget_control, id, get_value = orderString
  id = widget_info(event.top, find_by_uname='LevelsBatchText')
	widget_control, id, get_value = levelString
  id = widget_info(event.top, find_by_uname='FamilyBatchComboBox')
	family = widget_info(id, /combobox_gettext)
  id = widget_info(event.top, find_by_uname='AllLevelsBatchToggle')
	calcAllLevels = widget_info(id, /button_set)
	id = widget_info(event.top, find_by_uname='InputSplitImageNameText')
	widget_control, id, get_value = splitImageName
  id = widget_info(event.top, find_by_uname='InputSplitSizeText')
  widget_control, id, get_value = splitSizeString
  id = widget_info(event.top, find_by_uname='inputSplitToggle')
  splitFile = widget_info(id, /button_set)
  
	order = 0
	splitSize = 0

  if splitFile eq 1 then begin
    on_ioerror, bad_size
    splitSize_arr = fix(splitSizeString)
    splitSize = splitSize_arr[0]
  endif else begin
    if (strlen(filelistName) eq 0) then begin
      ans = Dialog_Message('Input file list of images is missing', /error, title='Wavelet error')
      return
    endif
  endelse
  
	on_ioerror, bad_order
	order = fix(orderString)
	on_ioerror, bad_level
	levels_arr = fix(levelString)
	levels = levels_arr[0]
	names = strarr(1)

	if (strlen(folder) eq 0) then begin
		ans = Dialog_Message('Output folder for coefficients is missing', /error, title='Wavelet error')
		return
	endif

	; reset error_handler
	on_ioerror, NULL

  if splitFile eq 1 then begin
    envi_open_file, splitImageName, r_fid = fid
    nrs_split_image, fid, splitSize, list = list
  endif else begin
  	filename = ''
  	openr, unit, filelistName, /get_lun
  	fileInfo = fstat(unit)
  	filelen = fileInfo.size
    while (~eof(unit)) do begin
      readf, unit, filename    ; read the next filename
      if n_elements(list) eq 0 then list = [filename] $
      else list = [list, filename]
    endwhile
    free_lun, unit
  endelse

  nrItems = n_elements(list)
  
	; initialise tranquilizer
	envi_report_init,["Performing multiple wavelet transformations",$
                      "This can take up to a few hours"],$
						base = batch_loop, title = "Batch progress"
	envi_report_inc, batch_loop, nrItems
	for li = 0, nrItems - 1 do begin
		envi_report_stat, batch_loop, li, nrItems - 1, cancel = cancel

		; calculate the wavelet coefficients
		wavelet_calc, list[li], '', folder, family, order, levels, names, inter, crystals
	endfor

	envi_report_init, base = batch_loop, /finish

	goto, done

bad_order:
	if (order le 0) then begin
		ans = Dialog_Message('Order must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif

bad_level:
	if (levels le 0) then begin
		ans = Dialog_Message('Level must be a none-zero integer number', /error, title='Wavelet error')
		return
	endif
	
bad_size:
  if (splitSize le 0) then begin
    ans = Dialog_Message('Image size must be a none-zero integer number', /error, title='Wavelet error')
    return
  endif

done:
	ans = Dialog_Message('Wavelet calculation finished', /information, title='Wavelet Information')

end
