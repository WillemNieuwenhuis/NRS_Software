; print the wavelet summary to multiline text field
pro printSummary, smy, smySub, names, field
	; write the output as text
	; first the header
	header=['crystal','mean','median','var','MAD','SD','min','max','energy(%)']
	fmtString = '(3(a-12), a-18, 3(a-12), 2(a-9))'
	headStr = string(header, format=fmtString)
	widget_control, field, set_value = headStr
	fmtString = '(2(f12.3), f18.3, 4(f12.3), f9.5)'
	lvl = 0
	for cc = 0, n_elements(names) - 1 do begin
		smyStr = string(names[cc], format='(a-7)') + string(smy[*,cc], format=fmtString)
		widget_control, field, set_value = smystr, /append
		if ((cc + 1) mod 3 eq 0) then begin
			; write the subtotals per level
			fmtLvl = '(i2.2)'
			smyStr = 'level' + string(lvl + 1, format=fmtLvl) + string(smySub[*,lvl], format=fmtString)
			widget_control, field, set_value = smystr, /append
			lvl +=1
		endif
	endfor

end

function handleSummaryClose, event
	; Close the window
	widget_control, event.top, /destroy
end

pro summaryWindow, summary, smySubTotals, names
;	Resolve_Routine, 'summaryWindow_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

	lines = n_elements(summary[1, *]) + n_elements(smySubTotals[1, *])	; count all lines
	; Form definition
	summary_form = Widget_Base( UNAME='summary_form', /col  $
		,TITLE='Wavelet coefficients statistics')

	field = widget_text(summary_form, ysize = lines + 2, xsize = 110, font='Courier*8', /sensitive)

	buttonPanel = widget_base(summary_form, /row, /align_right)
	closeButton = widget_button(buttonPanel, value = 'Close', uname = 'summaryCloseButton' $
			, event_func = 'handleSummaryClose' $
			)

	printSummary, summary, smySubTotals, names, field

	;  Create and display the form
	Widget_Control, /REALIZE, summary_form

	XManager, 'summaryWindow', summary_form, /NO_BLOCK

end