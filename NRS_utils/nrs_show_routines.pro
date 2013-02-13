function nrs_find_sav_routines, savefile, csv_file = csv_file
  compile_opt idl2

  sav_obj = obj_new('IDL_Savefile',savefile)
  proc = sav_obj->Names(/proc)
  func = sav_obj->Names(/func)
  
  if n_elements(csv_file) gt 0 then begin
    openw, lun, csv_file, /GET_LUN
    printf, lun, 'Type,name'
    printf, lun, proc, format = '("procedure,",a)'
    printf, lun, func, format = '("function,",a)'

    close, lun
    free_lun, lun
  endif
  return, [proc, func]
end

pro nrs_routines_in_sav_gui_define_buttons, button_info
  envi_define_menu_button, button_info, value = 'Show SAV file routines', $
    uvalue = 'Show SAV file routines', event_pro = 'nrs_routines_in_sav_gui', $
    ref_value = 'NRS', position = 'first'
end

pro nrs_routines_in_sav_gui_event, event
  compile_opt idl2

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_routines_in_sav_gobutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        nrs_routines_in_sav_handleSave, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_routines_in_sav_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end
    else:
    endcase

end

pro nrs_routines_in_sav_work, event, show_list = show_list, save_csv = save_csv
  compile_opt idl2
  
  show_list = keyword_set(show_list)
  save_csv = keyword_set(save_csv)
  
  if ~(show_list || save_csv) then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_routines_in_sav_refstack')
  widget_control, fld, get_value = savefile
  
  savefile = strtrim(savefile[0], 2)
  if strlen(savefile) eq 0 then return
  
  fld = widget_info(event.top, find_by_uname = 'nrs_routines_in_sav_outputFile')
  widget_control, fld, get_value = csv_file
  
  csv_file = strtrim(csv_file[0], 2)
  if ~save_csv || (strlen(csv_file) eq 0) then void = temporary(csv_file)
  
  list = nrs_find_sav_routines(savefile, csv_file = csv_file)
  
  if show_list && (n_elements(list) gt 0) then begin
    list = list[sort(list)]
    list = strjoin(list, string(13b) + string(10b))
    fld = widget_info(event.top, find_by_uname = 'nrs_routines_in_sav_wl')
    widget_control, fld, set_value = list
  endif else widget_control, fld, set_value = ['']
end

pro nrs_routines_in_sav_load, event
  compile_opt idl2
  
  nrs_routines_in_sav_work, event, /show_list
end

pro nrs_routines_in_sav_handleSave, event
  compile_opt idl2

  nrs_routines_in_sav_work, event, /save_csv
end

pro nrs_routines_in_sav_gui, event
  compile_opt idl2

  wide = 30
  label_width = 100
  label_wide_width = label_width + wide
  text_width =  60
  text_small_width = 10
  num_width =   15

  nrs_routines_in_sav_contentPanel = widget_base(uname = 'nrs_routines_in_sav_contentPanel'  $
    , /col  $
    , tab_mode = 1 $
    , TITLE = 'List contents of SAV file')

  nrs_routines_in_sav_mainPanel = widget_base(nrs_routines_in_sav_contentPanel, /frame, /col)

  nrs_routines_in_sav_refstack = cw_dirfile(nrs_routines_in_sav_mainPanel $
                , title = 'Input SAV file' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_wide_width $
                , uname = 'nrs_routines_in_sav_refstack' $
                , event_pro = 'nrs_routines_in_sav_load' $
              )

  nrs_routines_in_sav_label = widget_label(nrs_routines_in_sav_mainPanel $
                , value = 'Routines in SAV file' $
                , uname = 'nrs_routines_in_sav_label' $
                , xsize = label_wide_width $
                , /align_left $
              )
                
  nrs_routines_in_sav_wl = widget_text(nrs_routines_in_sav_mainPanel $
                , uname = 'nrs_routines_in_sav_wl' $
                , ysize = 25 $
                , /editable $
                , /scroll $
              )
  nrs_routines_in_sav_output_panel = widget_base(nrs_routines_in_sav_contentPanel, /frame, /col)
  nrs_routines_in_sav_outputFile = cw_dirfile(nrs_routines_in_sav_output_panel $
        , uname = 'nrs_routines_in_sav_outputFile' $
        , style = 'file' $
        , title = 'Output CSV file (optional)' $
        , xsize = text_width $
        , xtitlesize = label_wide_width $
        )

  nrs_gui_createButtonPanel, nrs_routines_in_sav_contentPanel $
                , ok_uname = 'nrs_routines_in_sav_gobutton', ok_value = 'Save CSV', ok_tooltip = 'Save the list of routines to a file' $
                , cancel_uname = 'nrs_routines_in_sav_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, nrs_routines_in_sav_contentpanel

  ; Initialize stuff

  XManager, 'nrs_routines_in_sav_gui', nrs_routines_in_sav_contentPanel, /no_block
end
