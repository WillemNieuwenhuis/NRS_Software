function cw_groupbox, parent, group_title = group_title, uname = uname  
  tlb = widget_base(parent, /col, title = 'fancy frame')
    
  bulletinboardbase = widget_base(tlb $
                      )
  label = widget_label(bulletinboardbase, value = group_title, xoffset = 5)
  labelgeometry = widget_info(label, /geometry)
  labelysize =  labelgeometry.ysize
  fancybase = widget_base(bulletinboardbase, /column, /frame $
                          , uname = uname $
                          , yoffset = labelysize / 2, ypad = 5, xpad = 5)
  label = widget_label(bulletinboardbase, value = group_title, xoffset = 5)

  return, fancybase    
end