function inform_handleAboutClose, event
	; Close the window
	widget_control, event.top, /destroy
end

pro inform_AboutWindow
	; Form definition
	inform_about_form = Widget_Base( UNAME='inform_about_form', /col  $
		,TITLE='About...')

	field = widget_text(inform_about_form, ysize = 26 + 2, xsize = 105, font='Courier*8', /sensitive)

	inform_buttonPanel = widget_base(inform_about_form, /row, /align_right)
	inform_CloseButton = widget_button(inform_buttonPanel, value = 'Close', uname = 'inform_CloseButton' $
			, event_func = 'inform_handleAboutClose' $
			)

	widget_control, field, /append, set_value = 'INFORM_Prospect - The INvertible FOrest Reflectance Model coupled with PROSPECT'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Original concept and implementation of INFORM: Clement Atzberger, 1999'
	widget_control, field, /append, set_value = 'INFORM modifications and validation: Martin Schlerf, 2004-2007'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Converted from Matlab to IDL: Willem Nieuwenhuis, 2008'
	widget_control, field, /append, set_value = 'Added GUI in IDL: Willem Nieuwenhuis, 2008'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'INFORM (Atzberger, 2000; Schlerf&Atzberger, 2006) simulates the bi-directional reflectance'
	widget_control, field, /append, set_value = 'of forest stands between 400 and 2500 nm. INFORM is essentially an innovative combination of'
	widget_control, field, /append, set_value = 'FLIM (Rosema et al., 1992), SAIL (Verhoef, 1984), and PROSPECT (Jacquemoud et al. 1996)'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Atzberger, C. 2000: Development of an invertible forest reflectance model: The INFOR-Model.'
	widget_control, field, /append, set_value = 'In: Buchroithner (Ed.): A decade of trans-european remote sensing cooperation. Proceedings'
	widget_control, field, /append, set_value = 'of the 20th EARSeL Symposium Dresden, Germany, 14.-16. June 2000: 39-44.'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Schlerf, M. & Atzberger, C. (2006): Inversion of a forest reflectance model to estimate biophysical'
	widget_control, field, /append, set_value = 'canopy variables from hyperspectral remote sensing data. Remote Sensing of Environment, 100: 281-294'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Rosema, A., Verhoef, W., Noorbergen, H. 1992: A new forest light interaction model in support of forest'
	widget_control, field, /append, set_value = 'monitoring. Remote Sensing of Environment, 42: 23-41.'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B. (1996): Estimating leaf'
	widget_control, field, /append, set_value = 'biochemistry using the PROSPECT leaf optical properties model, Remote Sens. Environ., 56:194-202.'
	widget_control, field, /append, set_value = ''
	widget_control, field, /append, set_value = 'Verhoef, W. 1984: Light scattering by leaf layers with application to canopy reflectance modeling: The'
	widget_control, field, /append, set_value = 'SAIL model. Remote Sensing of Environment, 16: 125-141.'

	;  Create and display the form
	Widget_Control, /REALIZE, inform_about_form

	XManager, 'inform_AboutWindow', inform_about_form, /NO_BLOCK

end