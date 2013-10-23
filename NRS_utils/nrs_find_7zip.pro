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
