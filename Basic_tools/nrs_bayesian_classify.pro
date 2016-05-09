;+
; :description:
;    Load a table with probability lookup. The first column contains the lookup value.
;    The other columns contain the probabilties for each class, either as percentages (0-100)
;    or as probabilities (0-1). The data is normalised to probabilities (0-1).
;    The header line contains the class names.
;    The first item in the header is not evaluated.
;    The output table contains the probabilities as (1 + nr_output_classes) x nr_lookup_values
;
; :params:
;    tbl : in, required
;      The CSV text table
;
; :keywords:
;    prob_data : out
;      The probability lookup table as 2-D matrix; this will be empty if the file is not found
;    class_names : out
;      The class names of all the probabilities
;
; :author: nieuwenhuis
; :history:
;   December 2015 : created
;-
pro nrs_bayesian_load_prob, tbl, prob_data = prob_data, class_names = class_names
  compile_opt idl2, logical_predicate

  fi = file_info(tbl)
  if ~fi.exists then begin
    prob_data = []
    return
  endif
  
  data = nrs_read_csv(tbl, header = class_names)
  nrcolumns = n_tags(data)
  class_names = class_names[1:-1]

  nrSlices = n_elements(data.(0))
  prob_data = fltarr(nrcolumns, nrSlices)
  for c = 0, nrcolumns - 1 do begin
    prob_data[c, *] = data.(c)
  endfor
  
  ; simple normalization to probabilities if needed
  ; but leave the lookup value intact (column 0)
  mnx = max(prob_data[1:-1,*])
  if mnx gt 1.0 then begin
    prob_data[1:-1,*] /= 100.0
    prob_data[1:-1,*] = ((prob_data[1:-1,*] > 0.0) < 1.0)
  endif
  
end

;+
; :description:
;    Load a table with prior probabilities. 
;    There is one column for each evidence, each column containing the probabilities,
;    either as percentages (0-100) or as probabilities (0-1). The data is normalised
;    to probabilities (0-1).
;    The header line contains the evidence names.
;    The output table contains the prior probabilities as nr_classes x nr_locations
;
; :params:
;    tbl : in, required
;      The CSV text table
;
; :keywords:
;    prior_data : out
;      The prior probability table as 2-D matrix
;    class_names : out
;      The names of all the classes
;
; :author: nieuwenhuis
; :history:
;   December 2015 : created
;-
pro nrs_bayesian_load_prior, tbl, prior_data = prior_data, class_names = class_names
  compile_opt idl2, logical_predicate

  data = nrs_read_csv(tbl, header = class_names)
  nrcolumns = n_tags(data)

  nrRecords = n_elements(data.(0))
  prior_data = fltarr(nrcolumns, nrRecords)
  for c = 0, nrcolumns - 1 do begin
    prior_data[c, *] = data.(c)
  endfor

  ; simple normalization to probabilities if needed
  mnx = max(prior_data)
  if mnx gt 1.0 then prior_data /= 100.0
  
  prior_data = ((prior_data > 0.0) < 1.0)
end

;+
; :description:
;    Describe the procedure.
;
; :params:
;    evidence
;    prob_folder
;
; :keywords:
;    prior
;    outname
;    prog_obj
;    cancelled
;
; :author: nieuwenhuis
; :history:
;    December 2015: nieuwenhuis, created
;-
pro nrs_bayesian_classify, evidence, prob_folder, prior = prior, outname = outname $
                         , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  ; first read the measured data
  evid = nrs_read_csv(evidence, header = evi_names)
  nrEvidence = n_tags(evid)

  nr_locations = n_elements(evid.(0))
  evid_data = fltarr(nrEvidence, nr_locations)
  for c = 0, nrEvidence - 1 do begin
    evid_data[c, *] = evid.(c)
  endfor

  ; Read the conditional probabilities (expert knowledge)
  if n_elements(prob_folder) eq 0 then $
    prob_folder = file_dirname(evidence)
  prob_tables = prob_folder + path_sep() + evi_names + '.csv'

  prob_struct = {probabilities, ename : '', prob : ptr_new(), thematic : 0} 
  nrEvid = n_elements(prob_tables)
  all_prob = replicate(prob_struct, nrEvid)
  cn = []
  valid = bytarr(nrEvid)
  for t = 0, nrEvid - 1 do begin
    nrs_bayesian_load_prob, prob_tables[t], prob_data = pd, class_names = cn
    valid[t] = n_elements(pd) gt 0
    if ~valid[t] then continue
    
    all_prob[t].ename = evi_names[t]
    all_prob[t].prob = ptr_new(pd)
    all_prob[t].thematic = 0
  endfor
  ix = where(valid, cnt)
  if cnt ne nrEvid then begin
    void = error_message('Not all conditional probabilities could be loaded (do all evidences match their filenames?)', trace = 0)
    return
  endif
  
  if n_elements(cn) eq 0 then begin
    void = error_message('No conditional probabilities found, nothing to do', trace = 0)
    return
  endif
  
  ; read the prior probabilities if specified
  ; note: no check is made on equality of classes
  all_prior_prob = []
  if n_elements(prior) gt 0 then $
    nrs_bayesian_load_prior, prior, prior_data = all_prior_prob, class_names = class_pnames

  ; Start classifying
  nr_classes = n_elements(cn)
  post_prob = fltarr(nr_classes, nr_locations)
  expert_data = fltarr(nr_classes, nrevid)
  for l = 0, nr_locations - 1 do begin
    expert_data[*] = 0.0
    lu_values = evid_data[*, l]
    ; get all the conditional probabilities for all evidences
    for t = 0, nrevid - 1 do begin
      ; get the conditional probabilities for each evidence
      pdt = *(all_prob[t].prob)
      ; Find the measurement value in the LUT for the conditional probabilities
      ix = where(lu_values[t] lt pdt[0, *], cnt)
      ; get the class probabilities for the measurement
      if cnt eq 0 then expert_data[*, t] = 0.0000001 $
      else $ 
        expert_data[*, t] = pdt[1:-1, ix[0]]
    endfor
    lowval = where(expert_data eq 0, cnt_low) ; lower bound for stability in the calculations
    if cnt_low gt 0 then expert_data[lowval] = 0.0000001
    ; calculate the posterior probabilities for a location
    ; using Bayesian logic
    prior_prob = fltarr(nr_classes) + 1.0 / nr_classes
    if n_elements(all_prior_prob) gt 0 then prior_prob = all_prior_prob[*, l]
    post_pHa = nrs_bayesian_rule(expert_data, prior_prob)
    post_prob[*, l] = post_pHa 
  endfor
  ; Classify by finding max probability for each location
  max_val = max(post_prob, max_ix, dim = 1)
  max_ix = max_ix mod nr_classes
  sel_class = cn[max_ix]

  ; done classifying
  ; save the result
  outdata = evid
  for c = 0, nr_classes - 1 do begin
    outdata = create_struct(outdata, cn[c], post_prob[c, *])
  endfor
  outdata = create_struct(outdata, 'Class', sel_class)
  
  header = [evi_names, cn, 'Class']  
  if n_elements(outname) eq 0 then outname = getoutname(point_table, postfix = '_clf', ext = '.csv')
  write_csv, outname, header = header, outdata
  
  ; cleanup heap memory
  for t = 0, nrevid - 1 do begin
    pdt = all_prob[t].prob
    ptr_free, pdt
  endfor
end
  
