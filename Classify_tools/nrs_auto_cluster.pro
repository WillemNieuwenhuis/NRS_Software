;+
; :Description:
;    For a range of number of classes, classify an input stack with isodata clustering; then
;    for each classified map calculate the separability.
;
; :Params:
;    stack : in
;      Input image stack
;
; :Keywords:
;    run_range : in
;      Two-element array with the minimum and maximum number of classes
;    basename : in
;      Basename for the classified maps; each class map is postfixed with '_isoxxx'
;      where xxx is the number of classes.
;    mask_name : in, optional
;      If specified, use the mask as input in the isodata classification (equiv. with not classifying zeros)
;    stat_out : in, optional
;      If specified the base output name for all separability matrices (one for each classification)
;    outtable : in, optional
;      The name of the output table in which the separability values for all classifications are stored.
;      If not specified the name of the input stack serves as the template; the filename is 
;      postfixed with '_separ'
;    append : in, optional
;      If set append table output, instead of recreating the output tables
;    iterations : in
;      The number of iterations in the isodata clustering
;    prog_obj : in
;      A progress indicator object (type progressBar)
;    cancelled : out
;      If set indicates an error, or a user abort
;
; :Author: nieuwenhuis
;-
pro nrs_auto_cluster, stack, run_range = run_range, basename = basename $
                    , mask_name = mask_name $
                    , stat_out = stat_out $
                    , outtable = out_table $
                    , append = append $
                    , iterations = iterations $
                    , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
  cancelled = 1
  envi_open_file, stack, r_fid = fid, /no_realize, /no_interactive_query
  if fid eq -1 then begin
    void = dialog_message('Could not find or open input image stack', /error)
    return
  endif

  if n_elements(basename) eq 0 then basename = getOutname(stack, postfix = '_iso', ext = '.dat')
  if n_elements(outtable) eq 0 then outtable = getOutname(stack, postfix = '_separ', ext = '.csv')
  
  if n_elements(run_range) ne 2 then begin
    void = dialog_message('Specify minimum and maximum number of classes as [min, max]', /error)
    return
  endif
  
  envi_open_file, mask_name, r_fid = m_fid, /no_realize, /no_interactive_query
  if m_fid eq -1 then delvar, m_fid

  cancelled = 0
  nr_class_min = min(run_range, max = nr_class_max)
  nr_class_out = nr_class_max - nr_class_min + 1
  
  envi_file_query, fid, dims = dims, nb = nb
  
  if n_elements(iterations) eq 0 then iterations = 50
  change_thresh = 0.0   ; 0.0: make sure that classifcation is running through all iterations, and does not stop earlier
  iso_merge_pairs = 2   ; max number of classes to merge in an iteration
  iso_merge_dist = 0.0  ; prevent merging within an iteration
  iso_min_pixels = 1    ; min number of pixels in a class
  iso_split_smult = 1.0 ; multiplier for standard deviation
  iso_split_std = 0.0   ; don't use split option

  ; start outer progress indicator for the iso-cluster automation 
  nrs_set_progress_property, prog_obj, xs = 600, ys = 100
  prog_obj->Start, 0

  ; inner progress indicator for separability
  prog_obj_inner = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                      , ysize = 15, title = 'Calculate separability' $
                      , /fast_loop $
                      )
  nrs_set_progress_property, prog_obj_inner, xs = 650, ys = 150

  nrs_log_line, outtable, 'nr_classes,min_sep,avg_sep', append = append, /header ; write header
  for c = nr_class_min, nr_class_max do begin
    if nrs_update_progress(prog_obj, c - nr_class_min, nr_class_out, cancelled = cancelled) then return
    
    ; classify
    cnames = ['Unclassified', string(indgen(c) + 1, format = '("class ",i03)')]
    clnum = string(c, format = '(i03)')
    outname = getOutname(basename, postfix = clnum, ext = '.') 
    envi_doit, 'class_doit', fid = fid, dims = dims, class_names = cnames $
                           , m_fid = m_fid, m_pos = 0 $
                           , r_fid = fid_cl $
                           , method = 4 $ ; isocluster
                           , out_bname = string(c, format = '("cluster ", i0)') $
                           , out_name = outname $
                           , pos = indgen(nb) $
                           , /no_realize $  ; envi_doit keyword
                    ; unsupervised classification keywords 
                    , iterations = iterations $
                    ; iso cluster keywords 
                    , change_thresh = change_thresh, iso_merge_dist = iso_merge_dist $
                    , iso_merge_pairs = iso_merge_pairs, iso_min_pixels = iso_min_pixels $
                    , iso_split_smult = iso_split_smult, iso_split_std = iso_split_std $
                    , min_classes = c, num_classes = c

    cancelled = fid_cl eq -1
    
    if ~cancelled then $ 
      ; with the new classified map, calculate the separability
      nrs_class_separability, stack, outname, sep_avg = sep_avg, sep_min = sep_min $
                          , separ_matrix = separ $
                          , prog_obj = prog_obj_inner, cancelled = cancelled

    if cancelled then break
    
    nrs_log_line, outtable, string([c, sep_min, sep_avg], format = '(i3,",",i0,",",i0)'), append = append
    
    nrs_write_separ_mat, separ, outname = stat_out, class = c, append = append
  endfor
  
  if prog_obj_inner ne !null then $
    prog_obj_inner->Destroy
end

;+
; :Description:
;    Write the data in separ to comma-separated ascii file
;
; :Params:
;    separ : in
;      The dat to write to file
;
; :Keywords:
;    outname : in
;      Basename of the file. If the file name is empty nothing is written
;    class : in, optional
;      Number to use in the postfix of the filename
;    cnames : in, optional
;      The names of the classes, used for row / col header
;    append : in, optional
;      If specified and set appends the matrix output to the output file, otherwise overwrites it
;
; :Author: nieuwenhuis
;-
pro nrs_write_separ_mat, separ, outname = outname, class = class, cnames = cnames, append = append
  if n_elements(outname) eq 0 then return
  nr_classes = n_elements(cnames) 
  
  if n_elements(class) gt 0 then begin
    postfix = string(class, format = '("_cl",i03)')
    outname = getOutname(outname, postfix = postfix, ext = '.csv')
  endif
  openw, unit, outname, /get_lun, append = append
  sz = size(separ, /dim)
  ns = sz[0]
  nl = sz[1]
  frm_str = string(ns - 1, format = '("(i0,",i0,''(",",i0))'')')
  if nr_classes gt 0 then begin
    frm_str = string(ns - 1, format = '("(a,'','',i0,",i0,''(",",i0))'')')
    printf, unit, ',' + strjoin(cnames, ',')
  endif
  row_hdr = ''
  for l = 0, nl - 1 do begin
    if nr_classes gt 0 then row_hdr = cnames[l] + ','
    printf, unit, row_hdr + string(separ[*, l], format = frm_str)
  endfor 
  printf, unit, ''
  printf, unit, '----------'
  close, unit
end
