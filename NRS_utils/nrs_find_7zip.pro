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

;+
; :description:
;    Locate the folder with the unzipper 7-zip.
;    
;    <b>Note</b>: Windows only.
;     
;
; :returns:
;   Folder name with the 7z.exe; or empty string if not found.
;
; :author: nieuwenhuis
;-
function nrs_find_7zip
  compile_opt idl2, logical_predicate

  is64bit = !version.memory_bits eq 64
  prog_dir32 = 'c:\Program Files (x86)\7-Zip'
  prog_dir = 'c:\Program Files\7-Zip'
  exefile = '7z.exe'

  dir = file_search(prog_dir, exefile)
  if n_elements(dir) eq 1 && strlen(dir) eq 0 && is64bit then begin
    ; if not found then try 32-bit version
    dir = file_search(prog_dir32, exefile)
  endif
  
  return, dir[0]
end
