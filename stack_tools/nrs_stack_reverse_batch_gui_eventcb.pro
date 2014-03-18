pro nrs_stack_reverse_batch_use_folder_toggle, event
  if (event.id eq widget_info(event.top, find_by_uname='nrs_stack_reverse_batch_use_folder_button')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility folder and file list fields
    cpanel = widget_info(event.top, find_by_uname='nrs_stack_reverse_batch_folder')
    widget_control, cpanel, sensitive = ~isOn
    
    cpanel = widget_info(event.top, find_by_uname='nrs_stack_reverse_batch_filelist')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro nrs_stack_reverse_batch_handle_input, event
  compile_opt idl2, logical_predicate
  
;  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_refstack')
;  widget_control, val_fld, get_value = target
;
;  target_str = strtrim(target)
;  if strlen(target_str) eq 0 then return
;
;  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_combo')
;  stack_reverse = widget_info(val_fld, /combobox_gettext)
;
;  basename = getOutname(target_str, postfix = '_' + stack_reverse, ext = '.dat')
;  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_outputFile')
;  widget_control, val_fld, set_value = basename
end

pro nrs_stack_reverse_batch_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_batch_use_folder_button')
  useFilelist = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_batch_folder')
  widget_control, val_fld, get_value = folder
  folder = strtrim(folder, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_batch_filelist')
  widget_control, val_fld, get_value = filelist
  filelist = strtrim(filelist, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_batch_keep_bnames')
  keep_bnames = widget_info(val_fld, /button_set)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_stack_reverse_batch_outputFolder')
  widget_control, val_fld, get_value = outname
  outname = strtrim(outname, 2)
  
  images = filelist
  if ~useFilelist then begin
    if strlen(folder) eq 0 then begin
      void = error_message('Folder not specified!', traceback = 0, /error)
      return
    endif
    images = folder
  endif else begin
    if strlen(filelist) eq 0 then begin
      void = error_message('List file not specified!', traceback = 0, /error)
      return
    endif
    folder = ''
  endelse
  
  if strlen(outname) eq 0 then begin
    void = error_message('Output folder not specified!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  prog_obj = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Batch reverse stack layers" $
                        , /fast_loop $
                        )
  
  nrs_stack_reverse_batch, images, keep_bnames = keep_bnames, outfolder = outname $
                  , use_listfile = useFilelist, use_folder = ~useFilelist $
                  , prog_obj = prog_obj, cancelled = cancelled

  if obj_valid(prog_obj) then $
    prog_obj -> Destroy
end

