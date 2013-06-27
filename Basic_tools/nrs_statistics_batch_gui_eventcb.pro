pro nrs_statistics_batch_use_folder_toggle, event
  if (event.id eq widget_info(event.top, find_by_uname='nrs_statistics_batch_use_folder_button')) then begin
    isOn = widget_info(event.id, /button_set)
    ; toggle visibility folder and file list fields
    cpanel = widget_info(event.top, find_by_uname='nrs_statistics_batch_folder')
    widget_control, cpanel, sensitive = ~isOn
    
    cpanel = widget_info(event.top, find_by_uname='nrs_statistics_batch_filelist')
    widget_control, cpanel, sensitive = isOn
  endif
end

pro nrs_statistics_batch_handleOK, event
  val_fld = widget_info(event.top, find_by_uname = 'nrs_statistics_batch_use_folder_button')
  useFilelist = widget_info(val_fld, /button_set)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_statistics_batch_folder')
  widget_control, val_fld, get_value = folder
  folder = strtrim(folder, 2)

  val_fld = widget_info(event.top, find_by_uname = 'nrs_statistics_batch_filelist')
  widget_control, val_fld, get_value = filelist
  filelist = strtrim(filelist, 2)
  
  val_fld = widget_info(event.top, find_by_uname = 'nrs_statistics_batch_outputFile')
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
    void = error_message('Output name not specified!', traceback = 0, /error)
    return
  endif
  
    ; initialise tranquilizer
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Batch statistics" $
                        , /fast_loop $
                        )
  
  nrs_statistics_batch, images, outtable = outname $
                  , use_listfile = useFilelist, use_folder = ~useFilelist $
                  , prog_obj = progressBar, cancelled = cancelled

  progressBar -> Destroy
end

