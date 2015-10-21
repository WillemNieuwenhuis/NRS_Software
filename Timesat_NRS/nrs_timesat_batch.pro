; start timesat processing on a list of input files
; The file list is expected in a text file with one file per line
; the file is assumed to have a fully qualified path
pro nrs_timesat_batch, event, input_list = input_list
  compile_opt idl2, logical_predicate
  
  if n_elements(input_list) eq 0 then begin
    input_list = dialog_pickfile()
    if n_elements(input_list) eq 0 || strlen(strtrim(input_list, 2)) eq 0 then return
  endif
  
  files = []
  openr, lun, input_list, /get_lun
  line = '' ; force reading text file
  while ~eof(lun) do begin
    readf, lun, line
    if strlen(line) gt 0 then begin
      fi = file_info(line)
      if fi.exists then files = [files, line]
    endif
  endwhile
  free_lun, lun
  
  if n_elements(files) eq 0 then begin
    void = dialog_message('No files where found')
    return
  endif
  
  progressBar = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
    , ysize = 15, title = 'Progress TIMESAT analysis' $
    , /fast_loop $
    )

  foreach file, files do begin
    out_filename = getOutname(file, ext = '.dat')
    timesat_v11, file, out_filename, prog_obj = progressBar, [1, 2, 3, 4], 1, 1
  endforeach
  
  if progressbar ne !null then $
    progressBar->destroy

end
