;+
; :description:
;    Unzip an archive file.
;
; :returns:
;   0 if no errors occurred, > 0 if an error occurred
;
; :params:
;    zipfile : in
;      The name of the archive file
;
; :keywords:
;    outfolder : in, optional
;      Set the folder in which to extract the archive file
;    flatten : in, optional, default = 0 (off)
;      If set extracts the contents to a single folder, discarding empty folders
;      If cleared then extracts the entire folder structure
;    mask : in, optional
;      Specify a mask to select only specific archive files
;
; :author: nieuwenhuis
;-
function nrs_unzip, zipfile, outfolder = outfolder, flatten = flatten, mask = mask
  compile_opt idl2, logical_predicate
  
  unzipper = nrs_find_7zip()
  if strlen(unzipper) eq 0 then begin
    void = error_message('Could not find 7-zip')
    return, 0
  endif
  
  if n_elements(outfolder) gt 0 then begin
    if strlen(strtrim(outfolder, 2)) gt 0 then begin
      fi_od = file_info(outfolder)
      if fi_od.exists && ~fi_od.directory then begin
        void = error_message('Cannot extract: trying to use file as folder')
        return, 0
      endif
      if ~fi_od.exists then file_mkdir, outfolder
    endif else void = temporary(outfolder)
  endif
  
  cmd = unzipper
  cmd += keyword_set(flatten) ? ' e ' : ' x '
  cmd += zipfile
  if n_elements(outfolder) gt 0 then cmd+= ' -o' + outfolder
  if n_elements(mask) gt 0 && strlen(strtrim(mask, 2)) gt 0 then cmd += ' ' + mask + ' -r'
  cmd += ' -y'

  spawn, cmd, exit_status = es, /noshell, /hide
  
  return, es
end

