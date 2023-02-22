;+
; :Description:
;    Calculate the covariance matrices of all classes in the classified image (excluding unclassified)
;
; :Returns:
;   The covariance matrix for all classes (excluding the unclassified class)
; 
; :Params:
;    fid : in
;      The envi id of the image stack with the signatures
;    cldata : in
;      The data of the classified map already loaded
;    nr_class : in
;      The number of classes (including unclassfied!)
;
; :Keywords:
;    unclass : in, optional
;      The class number of the unclassified class
;    mean_vec : out
;      The mean vector of all classes
;    valid_covar : out
;      For each class indicate if the covariance matrix could be calculated
;
; :Author: nieuwenhuis
;-
function nrs_get_classes_covar, fid, cldata, nr_class, unclass = unclass, mean_vec = mean_vec $
                    , valid_covar = valid_covar $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 0
  if n_elements(unclass) eq 0 then unclass = -1
  
  envi_file_query, fid, nb = nb, dims = dims

  nrs_set_progress_property, prog_obj, title = 'Calculate class covariance matrices'
  
  covar_mat = dblarr(nb, nb, nr_class)
  mean_vec = dblarr(nb, nr_class)
  valid_covar = intarr(nr_class)
  for cl = 0, nr_class - 1 do begin
    if nrs_update_progress(prog_obj, cl, nr_class, cancelled = cancelled) then return, temporary(covar_mat)
    
    if cl eq unclass then continue ; skip unclassified
    
    ; collect signatures for class under investigation
    ix = where(cldata eq cl, count)
    if count lt nb + 1 then continue
    
    sig = dblarr(count, nb)
    for b = 0, nb - 1 do begin
      dd = envi_get_data(fid = fid, dims = dims, pos = [b])
      sig[*, b] = dd[ix]
    endfor

    covar_mat[*, *, cl] = temporary(nrs_covar_matrix_t(sig, mean = meanx))
    mean_vec[*, cl] = temporary(meanx)
    valid_covar[cl] = 1
  endfor
  
  return, temporary(covar_mat)
end

;+
; :Description:
;    Calculate the class separability. The separability is calculated using
;    the divergence formula of Swain and Davis ( Remote Sensing: The Quantitative
;    Approach, 1978, pag 168, formula 3.33).
;
; :Params:
;    stack : in
;      The image stack with the signatures
;    class_image :
;      The classified image
;
; :Keywords:
;    sep_avg : out
;      The average separability
;    sep_min : out
;      The minimum separability
;    separ_matrix : out
;      The complete separability matrix (nr_class x nr_class), ignoring the unclassified values
;    prog_obj : in, optional
;      A progress indicator object (ProgressBar class)
;    cancelled : out
;      When set, indicates an error, or user abort
;
; :Author: nieuwenhuis
;-
pro nrs_class_separability, stack, class_image $
                    , sep_avg = sep_avg, sep_min = sep_min $
                    , separ_matrix = separ $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, stack, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = dialog_message('Could not find or open input image stack', /error)
    return
  endif
  
  envi_open_file, class_image, r_fid = fid_cl, /no_realize, /no_interactive_query
  if fid_cl eq -1 then begin
    void = dialog_message('Could not find or open input classified image', /error)
    return
  endif
  
  envi_file_query, fid_cl, ns = ns_cl, nl = nl_cl, file_type = ft_cl $
                 , dims = dims $
                 , class_names = cnames, num_classes = nr_class
  if ft_cl ne 3 then begin ; must be 3 == 'ENVI Classification'
    ci = file_basename(class_image)
    void = dialog_message('File ' + ci + ' must be a classified image', /error)
    return
  endif

  envi_file_query, fid, ns = ns, nl = nl, nb = nb

  if (ns ne ns_cl) || (nl ne nl_cl) then begin
    void = dialog_message('Dimensions of input stack and classified image do not match', /error)
    return
  endif
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, title = 'Loading classified image', /start
  
  cldata = envi_get_data(fid = fid_cl, dims = dims, pos = [0])
  ix = where(strlowcase(cnames) eq 'unclassified', has_unclass)
  unclass = has_unclass ? ix[0] : -1
  envi_file_mng, id = fid_cl, /remove   ; close classified image again
  
  nrs_set_progress_property, prog_obj, title = 'Calculating covariance matrices'

  covar_mat = nrs_get_classes_covar(fid, cldata, nr_class, unclass = unclass $
                   , mean_vec = mean_vec, valid_covar = valid_covar $
                   , prog_obj = prog_obj, cancelled = cancelled $
                   )
  cdims = size(covar_mat, /dim)
  nr_mat = cdims[2]
  separ = dblarr(nr_mat, nr_mat)
  
  nrs_set_progress_property, prog_obj, title = 'Determining class separability', /start
  for m1 = 0, nr_mat - 2 do begin
    if nrs_update_progress(prog_obj, m1, nr_mat - 1, cancelled = cancelled) then return
    
    if ~valid_covar[m1] then continue
    inv1 = la_invert(covar_mat[*, *, m1], status = failed_outer, /double)
    if failed_outer then continue
    
    for m2 = m1 + 1, nr_mat - 1 do begin
      if ~valid_covar[m2] then continue
      inv2 = la_invert(covar_mat[*, *, m2], status = failed_inner, /double)
      if failed_inner then continue
      
      r1 = covar_mat[*, *, m1] - covar_mat[*, *, m2]
      r2 = inv2 - inv1
      r3 = inv1 + inv2
      rd = mean_vec[*, m1] - mean_vec[*, m2]
      r4 = rd ## rd
      r5 = trace(temporary(r1) ## temporary(r2)) + trace(temporary(r3) ## temporary(r4))
      separ[m1, m2] = r5 / 2 ; Dij
    endfor
  endfor 
  nrs_set_progress_property, prog_obj, /start   ; restart progress bar
  
  ix = where(separ gt 0, count)
  if count eq 0 then ix = lindgen(nr_mat * nr_mat)  ; do all
  sep_avg = round(mean(separ[ix]))
  sep_min = round(min(separ[ix]))
end

;+
; :Description:
;    Calculate the covariance matrix of transpose(X)
;
; :Returns:
;   The covariance matrix
;   
; :Params:
;    x  : in
;      The matrix
;
; :Keywords:
;    mean : out
;      The mean values vector
;
; :Author: nieuwenhuis
;-
function nrs_covar_matrix_t, x, mean = meanx
  compile_opt idl2, logical_predicate
  
  dims = size(x, /dimensions)
  meanx = total(x, 1) / dims[0]
  idx = rebin(transpose(lindgen(dims[1])), dims[0], dims[1])
  varxi = x - meanx[idx]

  mm = matrix_multiply(varxi, varxi, /atranspose)/ (dims[0] - 1)
  return, temporary(mm)
end

pro test_separ
  compile_opt idl2, logical_predicate, hidden

  stack = 'E:\NRS\Kees-Andrew\Korea\korea_1apr98_20dec09_mask2.img'
  class_image = 'E:\NRS\Kees-Andrew\Korea\k99.img'
  
  ; start separability
  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = 'Determine class separability' $
                        , /fast_loop $
                        )
  progressBar->Start
  
  t1 = systime(1)
  nrs_class_separability, stack, class_image, prog_obj = progressBar, cancelled = cancelled $
                        , sep_min = sep_min, sep_avg = sep_avg $
                        , separ_mat = separ_mat
                        
  progressBar->destroy
  
  if cancelled then return
  
  ; results:
  ; k10.img, 1348, 136
  ; k11.img, 1383, 151
  ; k20.img, 1707, 119
  ; k99.img,    0,  81  ; extra test needed
  print, sep_avg 
  print, sep_min
  t2 = systime(1)
  print, t2 - t1, format = '("Time elapsed: ",f0.3, " (s)")'
  openw, unit, 'E:\NRS\Kees-Andrew\Korea\separ_mat99.txt', /get_lun
  sz = size(separ_mat, /dim)
  ns = sz[0]
  nl = sz[1]
  frm_str = string(ns - 1, format = '("(i0,",i0,''(",",i0))'')')
  for l = 0, nl - 1 do begin
    printf, unit, separ_mat[*, l], format = frm_str
  endfor 
  close, unit
  
  
end
