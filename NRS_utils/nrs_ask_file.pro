pro nrs_ask_file, textID, title
  widget_control, textID, get_value=filename

  filename = DIALOG_PICKFILE(title=title, file=filename, /read)
  if strlen(filename) eq 0 then return

  widget_control, textID, set_value=filename
end

