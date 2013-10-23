function nrs_get_temporary_dir, root = root
  compile_opt idl2, logical_predicate
  
  if n_elements(root) eq 0 then root = getenv('IDL_TMPDIR')
  if strmid(root,strlen(root)-1) ne path_sep() then root += path_sep()
  pf = randomu(seed, 1) * 10000
  repeat begin
    dirname = root + 'tmp@#$%_' + string(pf, format = '(i04)')
    fi = file_info(dirname)
  endrep until ~fi.exists && ~fi.directory
  
  file_mkdir, dirname
  
  return, dirname
end

