pro nrs_stack_use_folder_toggle, event
  compile_opt idl2, logical_predicate
  
  if (event.id eq widget_info(event.top, find_by_uname='nrs_stack_use_folder_button')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility folder and file list fields
    cpanel = widget_info(event.top, find_by_uname='nrs_stack_folder')
    widget_control, cpanel, sensitive = ~isOn
    
    cpanel = widget_info(event.top, find_by_uname='nrs_stack_filelist')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro nrs_stack_handleOK, event
  compile_opt idl2, logical_predicate
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_use_folder_button')
  useFilelist = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_folder')
  widget_control, val_fld, get_value = folder
  folder = strtrim(folder, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_filelist')
  widget_control, val_fld, get_value = filelist
  filelist = strtrim(filelist, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_outputFile')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  if ~useFilelist then begin
    if strlen(folder) eq 0 then begin
      void = error_message('Folder not specified!', traceback = 0, /error)
      return
    endif
  endif else begin
    if strlen(filelist) eq 0 then begin
      void = error_message('List file not specified!', traceback = 0, /error)
      return
    endif
    folder = ''
  endelse
  
  if strlen(outname) eq 0 then begin
    void = error_message('Output name not specified!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Stack layers" $
                        , /fast_loop $
                        )
  
  nrs_stack_image, outname, folder = folder, list_file = filelist, prog_obj = progressBar, cancelled = cancelled
  
  progressBar -> Destroy
end

