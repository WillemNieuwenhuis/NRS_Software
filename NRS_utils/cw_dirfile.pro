;+
; :Description:
;   Create a compound widget which contains a label/combobox/button to
;   select either a folder or a file:<br><br>
;   <img src='images/cw_dirfile.jpg'>
;   <br><br>CW_DIRFILE optionally remembers upto 10 earlier selections (last in, first out).
;
; :Examples:
;   How to create the CW_DIRFILE shown above:<pre>
;     id = cw_dirfile(base, style='file', title='Input path:', value='/home/idl/', /history)
;     ...
;     ; other code
;     </pre>
;   How to retrieve the selected folder or file<pre>
;     cbid = widget_info(base, find_by_uname='cw_dirfile')
;     widget_control, cbid, get_value=file</pre>
;   
; :History:
;   Created: 2009-10-15, Willem Nieuwenhuis
;
;-

;+
; :Hidden:
;-
function cw_dirfile_addhist, state, file
  compile_opt hidden

  if state.cbid ne -1 then begin
    widget_control, state.cbid, get_value = list
    item_cnt = n_elements(list)
    ; find the item in the list
    found = 0
    index = item_cnt - 1
    for i = 0, n_elements(list) - 1 do begin
      found = (list[i] eq file)
      if found then begin
        index = i
        break
      endif
    endfor
    
    if not found then begin
      if item_cnt eq 10 then begin
        ; remove first item from list if there are already 10 items in the list
        widget_control, state.cbid, combobox_deleteitem = 0
        index -= 1
      endif
      ; append the new item
      widget_control, state.cbid, combobox_additem = file
      index += 1
    endif
  
    ; make the new item visible in the text part
    widget_control, state.cbid, set_combobox_select = index
    
    return, index
  endif
  if state.txtid ne -1 then begin
    widget_control, state.txtid, set_value = file
  endif
end

;+
; :Hidden:
;- 
pro on_cw_dirfile_btn, ev
  stateholder = widget_info(ev.handler, /child)
  widget_control, stateholder, get_uvalue = state

  if !prompt ne 'ENVI> ' && state.style eq 'envi' then $
    state.style = 'file'
  file = ''
  if state.cbid ne -1 then file = widget_info(state.cbid, /combobox_gettext)
  if state.txtid ne -1 then widget_control, state.txtid, get_value = file
  file = strtrim(file[0], 2)
  dt = state.diatitle
  case state.style of
    'directory': begin
      if file_test(file, /directory) ne 1 then file = ''
      if strlen(dt) gt 0 then $
        file = dialog_pickfile(/directory, path = file, title = dt) $
      else $
        file = dialog_pickfile(/directory, path = file)
    end
    'file': begin
      if file_test(file, /regular) ne 1 then file = ''
      if strlen(dt) gt 0 then $
        file = dialog_pickfile(file = file, filter = state.filter, title = dt) $
      else $
        file = dialog_pickfile(file = file, filter = state.filter)
    end
    'envi': begin
      envi_select, dims = dims, pos = pos, fid = fid, /no_dims
      file = ''
      if fid ne -1 then envi_file_query, fid, fname = file
    end
  endcase
  
  if file eq '' then return

  state.pos = ptr_new(-1)
  if n_elements(pos) gt 0 then state.pos = ptr_new(pos, /no_copy)
  epro = state.epro  ; remember the callback event handler
  index = cw_dirfile_addhist(state, file)
  widget_control, stateholder, set_uvalue = state, /no_copy

  ; execute the callback event handler
  if epro ne '' then begin
    cmdstr = epro + ', ev'
    dummy = execute(cmdstr)
  endif
end

;+
; :Hidden:
; 
; Remove the first item from the combobox list
; Used only once after creation. Upon creation of the combobox
; the initial value is set to a string that contains a meaningless
; but non-empty string to size the combobox (the XSIZE keyword has
; no effect). After the form has been created the value is removed.
;-
pro cw_dirfile_init, cbid
  compile_opt hidden
  
  widget_control, cbid, combobox_deleteitem = 0
end

;+
; Retrieve the spectral selection (selected bands). Returns an array
; with the indices of the selected bands, or -1 if no selection is available
;-
function cw_dirfile_getpos, base
  stateholder = widget_info(base, /child)
  widget_control, stateholder, get_uvalue = state
  
  return, *(state.pos)
end

;+
; :Hidden:
; 
; Update the textfield with the new path / file name
;-
pro cw_dirfile_set, base, value
  compile_opt hidden
  
  svalue = value     ; prevent changes reaching caller
  sz  = size(svalue)
  if sz[0] ne 7 then svalue = strtrim(value, 2)  ; make string value
  
  stateholder = widget_info(base, /child)
  widget_control, stateholder, get_uvalue = state
  
  pos = strpos(svalue, ':')
  if pos ge 0 then begin
    sn = (strsplit(svalue, ':', /extract))[1]
    pos = where(sn eq ['style','filter','title'], cnt)
  endif
  
  case pos of
    0 : state.style = sn
    1 : state.filter = sn
    2 : begin
           state.title = sn + ':'
           widget_control, state.lblid, set_value = state.title
         end
    else: index = cw_dirfile_addhist(state, svalue)
  endcase
  
  widget_control, stateholder, set_uvalue = state, /no_copy
end

;+
; :Hidden:
; 
; Retrieve the currently selected path / file name through the widget_control function
; 
;-
function cw_dirfile_get, base
  compile_opt hidden
  stateholder = widget_info(base, /child)
  widget_control, stateholder, get_uvalue = state
  if state.cbid ne -1 then return, widget_info(state.cbid, /combobox_gettext)
  if state.txtid ne -1 then begin
    widget_control, state.txtid, get_value = val
    return, val
  endif
end

;+
; :Hidden:
;-
function cw_dirfile_event, ev
  compile_opt hidden
  
  if( tag_names(ev, /structure_name) eq 'WIDGET_BUTTON' )then begin
    on_cw_dirfile_btn, ev
  endif
  
  return, ev
end

;+
; Generate the component
;
; :Params:
;   parent : in, required
;     The parent base in which to place the CW_DIRFILE
;
; :Keywords:
;   event_pro : in, optional
;     Event handler to call when selected folder or file is changed
;     
;   filter : in, optional
;     File filter (only when style=file)
;   history : in, optional {default = 0}
;     Indicate if a history of up to 10 items should be maintained
;   label : in, optional {default = '...'}
;     Text value on the browse button
;   style : in, optional {default = 'directory'}
;     - style='directory'   opens a folder browse dialog,
;     - style='file' opens a file selection dialog,
;     - style='envi' opens the ENVI style image selection dialog
;   title : in, optional
;     The label text
;   uname : in, optional{default='cw_dirfile'}
;     The user name of the CW_DIRFILE                                
;   value : in, optional
;     Initial selection for folder / file
;   xsize : in, optional
;     Suggest the width of the text part (in characters) to IDL 
;   xtitlesize : in, optional
;     Suggest the width of the label part to IDL
;   ysize : in, optional
;     Suggest the height of the UI component to IDL
;   _extra : in, optional
;     Passes on other keywords to the main panel
;
;-                     
function cw_dirfile, parent, $
    event_pro = epro, $
    dialogtitle = dialogtitle, $
    filter=filter, $
    history=history, $
    label=btnvaluein,  $
    style=style, $
    title=title, $
    uname=uname,  $
    value=textvaluein, $
    xsize=xsize,  $
    ysize=ysize,  $
    xtitlesize=title_xsize, $
    _extra=_ex
    
  if n_elements(style) eq 0 then style = 'directory'
  column = keyword_set(column)
  row = 1 - column
  allevents = 1 - keyword_set(noedit)

  if n_elements(epro) le 0 then epro = ''
  if n_elements(filter) eq 0 then filter = '*'
  
  textvalue = (n_elements(textvaluein) gt 0) ? textvaluein : ' '
  btnvalue = (n_elements(btnvaluein) gt 0) ? btnvaluein : '...'

  ; convert non-string value to strings.
  if (size(textvalue, /tname) ne 'STRING') then $
    textvalue = strtrim(textvalue, 2)
    
  if n_elements(ysize) eq 0 then ysize = 1
  if n_elements(xsize) eq 0 then xsize = 125
  if n_elements(uname) eq 0 then uname= 'cw_dirfile'
  if n_elements(history) eq 0 then history = 0
  
  base = widget_base(parent, /row, $
    event_func = 'cw_dirfile_event', $
    pro_set_value = 'cw_dirfile_set', $
    func_get_value = 'cw_dirfile_get', $
    uname = uname, _extra = _ex)
    
  if ( n_elements(tab_mode) ne 0 ) then widget_control, base, tab_mode = tab_mode
  
  label_id = -1
  if n_elements(title) gt 0 then begin
    if n_elements(title_xsize) gt 0 then $
      label_id = widget_label(base, value = title, xsize = title_xsize, uname = uname + '_label') $
    else $ 
      label_id = widget_label(base, value = title, uname = uname + '_label') 
  endif else title = ' '

  if n_elements(dialogtitle) eq 0 then dialogtitle = ''
  
  fmtstr = string(xsize * 1.875, format='(i0)')
  txt = 'X' + string('X', format = '(a' + fmtstr + ')')
  text = -1
  combo = -1
  if history eq 1 then begin
    combo = widget_combobox(base, value = txt, $
      notify_realize = 'cw_dirfile_init', $
      event_pro = epro, $
      /editable, $
      uname = uname + '_combo')
  endif else begin
    text = widget_text(base, value = textvalue, $
      xsize = xsize, $
      event_pro = epro, $
      /editable, $
      uname = uname + '_text')
  endelse
  
  btnt = widget_button(base, value = btnvalue, ysize = ysize, uname = uname + '_btn', tab_mode = 0)
    
  ; save our internal state in the first child widget
  state   = {     $
    base: base,          $
    epro: epro,         $
    pos: ptr_new(), $
    cbid: combo,  $
    txtid: text, $
    lblid: label_id, $
    init_val:textvalue, $
    ncm: 0,              $
    title: title,        $
    diatitle: dialogtitle,  $
    filter: filter, $
    style: style $
    }
  widget_control, widget_info(base, /child), set_uvalue = state, /no_copy
  
  return, base
end
