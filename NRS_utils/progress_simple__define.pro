;*****************************************************************************************************
;+
; NAME:
;       progress_simple__define
;
; PURPOSE:
;
;       Creates a simple progress indicator for indicating the progress of a looping
;       operation in IDL. The indication is pure text-based on the console.
;       It is not possible to break out of the loop.
;       It is meant to replace the regular progressbar object in places where some reporting
;       is useful, but only in text form (f.e long batch processes) and not all reporting
;       is necessary.
;       It uses the same interface as the PROGRESSBAR object by David Fanning, Ph.D.
;       (see: http://www.dfanning.com)
;
; AUTHOR:
;
;       Willem Nieuwenhuis
;       Department of Natural Resources
;       ITC Faculty
;       Twente University
;       E-mail: w.nieuwenhuis@utwente.nl
;
; CATEGORY:
;       NRS Utilities
;
; CALLING SEQUENCE:
;
;       progressBar = Obj_New("progress_simple")
;
; KEYWORDS:
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress ..."
;
; KEYWORDS: (compatibility only, no function)
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       BACKGROUND:    The name of the background color of the progress bar. By default, "black".
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       FAST_LOOP:     Set this keyword if what you are doing in the loop doesn't involve
;                      any color operations and you want the progress bar to update as fast
;                      as possible. With this keyword set, the program will eliminate extra
;                      calls to FSC_COLOR, which can be slow if you are calling it, say,
;                      10,000 times!
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.
;
;
; PROCEDURE:
;
;       The user is responsible for starting, updating, checking for CANCEL events, and
;       destroying the progress indicator. The sequence of commands might look
;       like this:
;
;          progressBar = Obj_New("progress_simple")
;          progressBar -> Start
;          for j=0,9 do begin
;             ; some (lengthly) operation
;             progressbar -> update, (j+1)*10
;          endfor
;          progressBar -> Destroy
;
;       See the example program at the end of this file for a working example of code.
;
; METHODS:
;
;       CHECKCANCEL: This function method always returns 0 (no break)
;
;          cancelled = progressBar -> CheckCancel()
;          IF cancelled THEN progressBar->Destroy
;
;       DESTROY: Destroys the progress bar widgets as well as the object.
;
;          progressBar -> Destroy
;
;       GETPROPERTY: Gets certain properties of the object. (no properties supported)
;
;          progressBar -> GetProperty, Color=currentColor
;
;       SETPROPERTY: Allows the user to set certain properties of the object. (no properties supported)
;
;          progressBar -> SetProperty, Color='green'
;
;       START: Puts the progress bar on the display and enables it to receive events.
;
;          progressBar -> Start
;
;       UPDATE: Updates the progress bar. Requires on argument, a number between 0
;          and 100 that indicates the percent of progress bar that should be filled
;          with a color. The optional TEXT keyword is ignored.
;
;          progressBar -> Update, 50
;          progressBar -> Update, 50, Text='Operation 50% completed...'
;
; DEPENDENCIES:
;
;       No dependencies
;
; MODIFICATION HISTORY:
;
;       Written by:  Willem Nieuwenhuis, March 2013.
;-
;+
; :description:
;    This procedure makes sure the object is destroyed when the progress bar is destroyed.
;
; :params:
;    tlb : in
;      Not used
;
; :author: nieuwenhuis
;-
pro progress_simple_cleanup, tlb
  compile_opt idl2

  obj_destroy, self
end

;+
; :description:
;    This function is the standard ERROR_MESSAGE error handling functionality.
;
; :params:
;    themessage : in
;      The error message to display
;
; :keywords:
;    error : in
;    informational : in
;    traceback : in
;    noname : in
;    title : in
;    _extra : in
;
; :author: nieuwenhuis
;-
function progress_simple_error_message, themessage, error = error, informational = information, $
   traceback = traceback, noname = noname, title = title, _extra = extra
  compile_opt idl2

  on_error, 2

  ; check for presence and type of message.
  if n_elements(themessage) eq 0 then themessage = !error_state.msg
  s = size(themessage)
  messagetype = s[s[0]+1]
  if messagetype ne 7 then begin
     message, "The message parameter must be a string.", _extra=extra
  endif

  ; get the call stack and the calling routine's name.
  help, calls=callstack
  callingroutine = (strsplit(strcompress(callstack[1])," ", /extract))[0]

  ; are widgets supported?
  widgetssupported = ((!d.flags and 65536l) ne 0)
  if widgetssupported then begin
    ; if this is an error produced with the message command, it is a trapped
    ; error and will have the name "idl_m_user_err".
    if !error_state.name eq "idl_m_user_err" then begin
    if n_elements(title) eq 0 then title = 'trapped error'
  
    ; if the message has the name of the calling routine in it,
    ; it should be stripped out. can you find a colon in the string?

    ; is the calling routine an object method? if so, special processing
    ; is required. object methods will have two colons together.
    doublecolon = strpos(themessage, "::")
    if doublecolon ne -1 then begin
      prefix = strmid(themessage, 0, doublecolon+2)
      submessage = strmid(themessage, doublecolon+2)
      colon = strpos(submessage, ":")
      if colon ne -1 then begin
        ; extract the text up to the colon. is this the same as
        ; the callingroutine? if so, strip it.
        if strmid(themessage, 0, colon + strlen(prefix)) eq callingroutine then $
          themessage = strmid(themessage, colon + 1 + strlen(prefix))
        endif
      endif else begin
        colon = strpos(themessage, ":")
        if colon ne -1 then begin
          ; extract the text up to the colon. is this the same as
          ; the callingroutine? if so, strip it.
          if strmid(themessage, 0, colon) eq callingroutine then $
            themessage = strmid(themessage, colon+1)
          endif
      endelse

      ; add the calling routine's name, unless noname is set.
      if keyword_set(noname) then begin
        answer = dialog_message(themessage, title = title, _extra = extra, $
                                error = error, information = information)
      endif else begin
        answer = dialog_message(strupcase(callingroutine) + ": " + $
                                themessage, title = title, _extra = extra, $
                                error = error, information = information)
      endelse
    endif else begin
      ; otherwise, this is an idl system error.
      if n_elements(title) eq 0 then title = 'System Error'
      if strupcase(callingroutine) eq "$MAIN$" then $
        answer = dialog_message(themessage, _extra = extra, title = title, $
                                error = error, information = information) else $
        if keyword_set(noname) then begin
          answer = dialog_message(themessage, _extra = extra, title = title, $
                                  error = error, information = information)
        endif else begin
          answer = dialog_message(strupcase(callingroutine) + "--> " + $
               themessage, _extra=extra, title=title, $
               error=error, information=information)
        endelse
    endelse
  endif else begin
    message, themessage, /continue, /noprint, /noname, /noprefix, _extra = extra
    print, '%' + callingroutine + ': ' + themessage
    answer = 'OK'
  endelse

  ; provide traceback information if requested.
  if keyword_set(traceback) then begin
    help, /last_message, output = traceback
    print,''
    print, 'Traceback Report from ' + strupcase(callingroutine) + ':'
    print, ''
    for j = 0, n_elements(traceback) - 1 do print, "     " + traceback[j]
  endif

  return, answer
end


;+
; :description:
;   This is the event handler for the program. Only included for compatibility.
;
; :params:
;    event : in
;      Unused
;
; :author: nieuwenhuis
;-
pro progress_simple_event, event
end

;+
; :description:
;    Alwasy indicate that no button is pressed
;
; :keywords:
;    accept : out
;    cancel : out
;
; :author: nieuwenhuis
;-
function progress_simple::checkbutton, accept = accept, cancel = cancel
  compile_opt idl2
  
  accept = 0
  cancel = 0
   
  return, 0
end

;+
; :description:
;    Indicate the cancel button was not pressed
;
; :author: nieuwenhuis
;-
function progress_simple::checkcancel
  compile_opt idl2

  return, 0
end

;+
; :description:
;    Destroy the object
;
; :author: nieuwenhuis
;-
pro progress_simple::destroy
  compile_opt idl2

  obj_destroy, self
end

;+
; :description:
;    Allows user to get various progress bar properties.
;
; :syntax:
;   progressbar -> GetProperty, Color=currentColor
; 
; :keywords:
;    color : out
;      Return the value of the color property
;    fast_loop : out
;      Return the value of the fast_loop property
;    text : out
;      Return the value of the text property
;    tlb_id : out
;      Return -1
;    percent : out
;      Return the value of the percent property
;    background : out
;      Return te value of the background color property
;
; :author: nieuwenhuis
;-
pro progress_simple::getproperty, color=color, fast_loop=fast_loop, text=text, $
                              tlb_id=tlb_id, percent=percent, background=background

  compile_opt idl2

  if arg_present(background) then background = self.background
  if arg_present(color) then color = self.color
  if arg_present(fast_loop) then fast_loop = self.fast
  if arg_present(percent) then percent = self.percent
  if arg_present(text) then text = self.text
  if arg_present(tlb_id) then tlb_id = self.tlb

end

;+
; :description:
;    Allows user to set various progress bar properties.
;
; :keywords:
;    background : in
;      Set the value of the background property
;    cancel : in
;      Set the value of the cancel property
;    color : in
;      Set the value of the color property
;    fast_loop : in
;      Set the value of the fast_loop property
;    text : in
;      Set the value of the text property
;    title : in
;      Set the value of the title property
;    xoffset : in
;      Do nothing
;    yoffset : in
;      Do nothing
;
; :author: nieuwenhuis
;-
pro progress_simple::setproperty, $
   background=background, $
   cancel=cancel, $
   color=color, $
   fast_loop=fast_loop, $
   text=text, $
   title=title, $
   log_file=log_file, $
   xoffset=xoffset, $
   yoffset=yoffset

  compile_opt idl2

  if n_elements(background) ne 0 then self.background = background
  if n_elements(cancel) ne 0 then self.cancelflag = keyword_set(cancel)
  if n_elements(fast_loop) ne 0 then self.fast = fast_loop
  if n_elements(color) ne 0 then self.color = color
  if n_elements(text) ne 0 then self.text = text
  if n_elements(title) ne 0 then self.title = title
  if n_elements(log_file) ne 0 then self.log_file = log_file
end

;+
; :description:
;    Initialize the progress indicator
;
; :params:
;    initialpercent : in, optional
;      If set update the indicator to the percentage
;
; :author: nieuwenhuis
;-
pro progress_simple::start, initialpercent
  compile_opt idl2

  self.last_time = systime(/seconds)
  self.percent = 0.0
  if n_elements(initialpercent) ne 0 then self -> update, initialpercent
end

;+
; :description:
;    Updates the progress indicator.
;
; :params:
;    percent : in, required
;      A value between 0 and 100 that represents the percentage of the progress.
;
; :keywords:
;    text : in
;      Message text (not shown)
;    title : in
;      Title text (not shown)
;
; :author: nieuwenhuis
;-
pro progress_simple::update, percent, text = thetext, title = thetitle
  compile_opt idl2

  msg = ''
  if n_elements(thetext) gt 0 && thetext ne self.text then begin
    self.text = thetext
  endif
  if strlen(self.text) gt 0 then msg = self.text + ": "
  
  percent = round(percent * 10) / 10.0
  percent = 0 > percent < 100
  if percent ne self.percent then begin
    self.percent = percent
;    elap = systime(/seconds) - self.last_time
;    s_elap = nrs_sec_to_string(elap, /time, /date)
    s_elap = systime() + string(self.level, format='(" (Level ",i0,")")')
    msg = s_elap + ', ' + msg + string(self.percent, format = '(f0.1,"%")')
    if n_elements(self.log_file) gt 0 then begin
      unit = self.log_unit
      nrs_log_line, self.log_file, msg, /append, use_unit = unit
      if unit gt 0 then self.log_unit = unit
    endif    else print, msg 
  endif
end

;+
; :description:
;    Nothing to do in this cleanup routine.
;
; :author: nieuwenhuis
;-
pro progress_simple::cleanup
end

;+
; :description:
;    Implements a simple text based progress indicator functionality.
;
; :keywords:
;    percent : in
;      Initial percentage to display
;    start : in
;      Indicate start of progress
;    text : in
;      Set the text property
;    
; :Unsed keywords:
;    accept : in
;      No function
;    background : in
;      No function
;    color : in
;      No function
;    fast_loop : in
;      No function
;    group_leader : in
;      No function
;    nocancel : in
;      No function
;    title : in
;      No function
;    level : in
;      Indicates the level of the indicator (0 = outer level)
;    log_file : in
;      Name of the log file
;    xoffset : in
;      No function
;    xsize : in
;      No function
;    yoffset : in
;      No function
;    ysize : in
;      No function
;
; :author: nieuwenhuis
;-
function progress_simple::init, $
              accept=accept, $             ; set this keyword to get an accept button.
              background=background, $     ; the name of the background color of the progress bar.
              color=color, $               ; the name of the color of the progress bar.
              fast_loop=fast_loop, $       ; the user plans to use the progress bar in a fast loop.
              group_leader=group_leader, $ ; the identifier of the group leader widget.
              nocancel=nocancel, $         ; set this keyword to leave off the cancel button.
              percent=percent, $           ; initial percent of the progress bar. (only recognized if start used.)
              start=start, $               ; set this keyword if you wish to call the start method from init.
              text=text, $                 ; the message text to be written over the progress bar.
              title=title, $               ; the title of the top-level base widget.
              level=level, $               ; the level of progress if multiple loop are used, used for initial placement
              log_file = log_file, $       ; the name of the log file
              xoffset=xoffset, $           ; the x offset of the progress bar.
              xsize=xsize, $               ; the x size of the progress bar.
              yoffset=yoffset, $           ; the y offset of the progress bar.
              ysize=ysize                  ; the y size of the progress bar.

  compile_opt idl2
  
  if n_elements(background) eq 0 then self.background = "black" else self.background = background
  if n_elements(color) eq 0 then self.color = "red" else self.color = color
  self.fast = keyword_set(fast_loop)
  if n_elements(text) eq 0 then text = "Operation in progress ..."
  if n_elements(title) eq 0 then title = "Progress Bar"
  if n_elements(xsize) eq 0 then self.xsize = 150 else self.xsize = xsize
  if n_elements(ysize) eq 0 then self.ysize = 10 else self.ysize = ysize
  if n_elements(level) eq 0 then self.level = 0

  ; start it up?
  if n_elements(percent) eq 0 then percent = 0
  if keyword_set(start) then self -> start, percent

  return, 1
end

;+
; :description:
;    This is the progress_simple object's structure definition code.
;
; :author: nieuwenhuis
;-
pro progress_simple__define

   struct = { progress_simple, $      ; the object class name.
              background: "", $   ; the name of the background color of the progress bar.
              cancelflag: 0l, $   ; a flag to indicate that the cancel button was clicked.
              cancelid: 0l, $     ; the identifier of the cancel button.
              acceptid: 0l, $     ; the identifier of the accept button.
              color: "", $        ; the name of the color of the progress bar.
              colorindex: 0l, $   ; the color index number (set by a call to fsc_color).
              drawid: 0l, $       ; the identifier of the draw widget.
              fast: 0l, $         ; a "fast loop" flag.
              labelid: 0l, $      ; the identifier of the label widget.
              oldcolor: 0l, $     ; the color index of !p.color.
              percent: 0.0, $     ; a number between 0 and 100.
              r: 0b, $            ; the r value of !p.color.
              g: 0b, $            ; the g value of !p.color.
              b: 0b, $            ; the b value of !p.color.
              text: "", $         ; the text message to be written over the progress bar.
              title: "", $        ; the title of the top-level base widget.
              level: 0, $         ; The level of progress if multiple loop are used
              tlb: 0L, $          ; the identifier of the top-level base.
              wid: 0L, $          ; the window index number of the draw widget.
              xsize: 0L, $        ; the xsize of the progress bar.
              ysize: 0L, $        ; the ysize of the progress bar.
              last_time: 0L, $    ; previous time the progress was started 
              log_file: '', $     ; If specified will case all updates to be redirected to file
              log_unit: -1 $      ; handle to logfile if logfile is specified
            }
end


