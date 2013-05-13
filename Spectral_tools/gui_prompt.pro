; The event handler
PRO gui_prompt_event, event
	widget_control, event.id, GET_VALUE=userEntry, GET_UVALUE=pReturnValue
	*pReturnValue = userEntry
	widget_control, event.top, /DESTROY
END

; The widget creation code
FUNCTION gui_prompt, promptText, $
		TITLE=windowTitle, VALIDATE_TYPE=datatype, XSIZE=nCharacters, $
		XOFFSET=xoffset, YOFFSET=yoffset, _EXTRA=_extra
	if n_elements(promptText) eq 0 then return, ''
	; Optional keyword for validating user entries. The values are based
	; on IDL type ID's as seen in table in Online Help for SIZE function.
	if keyword_set(datatype) then begin
	    switch datatype of
	    1:    ; Integer types
	    2:
	    3:
	    12:
	    13:
	    14:
	    15: begin
	        validateLong = 1
	        break
	    end
	    4:    ; floating-point types
	    5: begin
	        validateReal = 1
	        break
	    end
	    else: returnValue = ''    ; default type is /STRING
	    endswitch
	endif
	if n_elements(windowTitle) eq 0 then winTitle = 'Entry Form'
	device, GET_SCREEN_SIZE=displayDims
	; By default, center the prompt (approximately) on the display
	if n_elements(xoffset) eq 0 then xoffset = displayDims[0] / 2
	if n_elements(yoffset) eq 0 then yoffset = displayDims[1] / 2
	; By default, set the entry textbox width to 20
	if n_elements(nCharacters) eq 0 then nCharacters = 20
	tlb = widget_base(TITLE=windowTitle, XOFFSET=xoffset, YOFFSET=yoffset)
	pReturnValue = ptr_new(/ALLOCATE_HEAP)
	wPromptBox = cw_field(tlb, TITLE=promptText, FLOATING=validateReal, $
	    LONG=validateLong, UVALUE=pReturnValue, $
	    /RETURN_EVENTS, XSIZE=nCharacters, _EXTRA=_extra)
	widget_control, tlb, /REALIZE
	xmanager, 'gui_prompt', tlb
	if n_elements(*pReturnValue) eq 0 $
	    then returnValue='' $
	else $
	    returnValue = strtrim(*pReturnValue, 2)
	ptr_free, pReturnValue
	return, returnValue
END