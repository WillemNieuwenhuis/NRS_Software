pro nrs_giacis_handleBrowseInput, event
  fld = widget_info(event.top, find_by_uname = 'nrs_giacis_input_text')
  widget_control, fld, get_value = filename
  
  filename = strtrim(filename, 2)
  if strlen(filename) eq 0 then return
  
	envi_open_file, filename, r_fid = fid, /no_realize, /no_interactive_query
	if fid eq -1 then return

	envi_file_query, fid, nb = nb, nl = nl, ns = ns, data_type = dt

	; update the UI controls
	txt_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_InfoText')
	widget_control, txt_fld, set_value = $
		'Dimensions: '	+ string(nl, format='(i0)') + ' rows, ' $
						+ string(ns, format='(i0)') + ' cols, ' $
						+ string(nb, format='(i0)') + ' bands, ' $
						+ 'type = ' + nrs_IDL_type_to_string(dt)

  fld = widget_info(event.top, find_by_uname = 'nrs_giacis_range_panel')
  widget_control, fld, sensitiv = (dt ne 1)

	; think about a default output name
	outfile_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_output_text')
	widget_control, outfile_fld, get_value = outfile
	if strlen(strtrim(outfile, 2)) eq 0 then begin
	  outfile = getOutname(filename, ext = '.')
		widget_control, outfile_fld, set_value = outfile
	endif
	
end

pro nrs_giacis_handleGo, event
  compile_opt idl2, logical_predicate

	; Collect all input, parameters and output from UI
	val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_input_text')
	widget_control, val_fld, get_value = src_filename

	val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_output_text')
	widget_control, val_fld, get_value = out_filename

  val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_range_min')
  widget_control, val_fld, get_value = vr_min_str

  val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_range_max')
  widget_control, val_fld, get_value = vr_max_str

	val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_SavGolText')
	widget_control, val_fld, get_value = win_s

  src_filename = strtrim(src_filename, 2)
  if strlen(src_filename) eq 0 then return
  
	out_filename = strtrim(out_filename, 2)
  if strlen(out_filename) eq 0 then begin
    void = dialog_message('You need to specify the output name', /error)
    return
  endif
  
  if strlen(strtrim(vr_min_str, 2)) gt 0 then vr_min = float(vr_min_str[0])
  if strlen(strtrim(vr_max_str, 2)) gt 0 then vr_max = float(vr_max_str[0])
  
	win_as = strsplit(win_s, '[],',/extract)
	win = fix(win_as)

	val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_UpEnvelCheck')
	forceUpEnvel = widget_info(val_fld, /button_set)

	val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_RegFitCheck')
	lastIterTIMESATfit = widget_info(val_fld, /button_set)

	txt_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_InfoText')

  ; start calculation
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Progress TIMESAT analysis' $
                        , /fast_loop $
                        )
  progressBar->Start

	nrs_timesat_idl, src_filename, out_filename $
	           , win, forceUpEnvel, lastIterTIMESATfit $
	           , prog_obj = progressBar, cancelled = cancelled $
	           , vr_min = vr_min, vr_max = vr_max $
	           , disp_win = txt_fld

  if progressBar ne !null then $
    progressBar->Destroy
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_giacis_output_text')
	widget_control, val_fld, set_value = ''	; clear output name to avoid reusing the name for a second run

end