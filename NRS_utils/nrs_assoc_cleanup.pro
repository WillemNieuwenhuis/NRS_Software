pro nrs_assoc_cleanup, unit, outname, prog_obj
  ; cleanup if some error
  close, unit  ; close assoc
  free_lun, unit
  file_delete, outname, /noexpand_path, /allow_nonexistent, /quiet
  if n_elements(prog_obj) gt 0 then prog_obj -> Destroy
  catch, /cancel
  ans = dialog_message(!error_state.msg, title = 'Error', /error)
  return
end

