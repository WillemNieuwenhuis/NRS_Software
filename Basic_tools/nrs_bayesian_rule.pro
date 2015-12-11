;+
; :description:
;    Determine the posterior probabilities according to the Bayesian rule.
;    Input is a 2-D array with expert probabilities (nr_classes x nr_measurements)
;
; :returns:
;    Posterior probabilities for each class
; 
; :params:
;    expert_prob : in, required
;      conditional probabilities, organised as: nr_classes x nr_measurements
;    prior_prob : in, optional
;      prior probabilities organised as: 1-D array dimension = nr_measurements
;
; :keywords:
;    normalize : in, optional, default = true
;      if true normalize the sum of measurement probabilities per class to one
;      if false skip the normalizing step
;
; :author: nieuwenhuis
; :history:
;   December 2015: created
;-
function nrs_bayesian_rule, expert_prob, prior_prob, normalize = normalize
  compile_opt idl2, logical_predicate
  
  propagate = ~keyword_set(no_propagate)
  
  nrclass = n_elements(expert_prob[*, 0])
  nrlayers = n_elements(expert_prob[0, *])
  if nrclass eq 0 then return, 0.0

  pEbHa = expert_prob
  if keyword_set(normalize) then begin
    ; Normalize step. This makes sure that for each class the sum of 
    ; the measurement probabilities (PEbHa) equals 1
    idx = indgen(nrclass * nrlayers) / nrclass
    ept = total(expert_prob, 1)
    pEbHa = expert_prob / ept[idx]
  endif
  
  if n_elements(prior_prob) ne nrlayers then prior_prob = fltarr(nrclass) + 1.0
  probability = prior_prob
  post_pHa = fltarr(nrclass)
  for c = 0, nrclass - 1 do begin
    for l = 0, nrlayers - 1 do begin
      pE = total(pEbHa[*, l] * probability)
      pHa = probability[c]
      pHaEb = pEbHa[c, l] * pHa / pE
      probability[c] = pHaEb
    endfor
    post_pHa[c] = probability[c]
  endfor
  
  return, post_pHa  ; dimension = nr_classes
end

function permute, ar, order
  compile_opt idl2, logical_predicate

  nrwid = n_elements(order)
  nrdim = n_elements(ar) / nrwid
  
  iy = rebin(transpose(indgen(nrdim)*nrwid), nrwid, nrdim)
  ix = rebin(order, nrwid, nrdim) + iy
  
  return, ix
end

pro test_bayes
  compile_opt idl2, logical_predicate
  
  ep = [ [0.125000, 0.208333, 0.375000, 0.291667] $
       , [0.133333, 0.100000, 0.500000, 0.266667]]
  pp = fltarr(4,2) + 1.0

;  ; WITHOUT propagating posterior probability  
;  ix = permute(ep, [0,1,2,3])
;  print, nrs_get_bayes_prob(ep[ix], pp)
;  
;  ix = permute(ep, [0,1,3,2])
;  print, nrs_get_bayes_prob(ep[ix], pp)
;  
;  ; WITH propagating posterior probability
;  ix = permute(ep, [0,1,2,3])
;  print, nrs_get_bayes_prob(ep[ix], pp, /prop)
;  
;  ix = permute(ep, [0,1,3,2])
;  print, nrs_get_bayes_prob(ep[ix], pp, /prop)
  
  ; EXPERT system (x=200, y=150; envir.dat)
  exp_prob = [ [0.2,0.8,0.0] $
    , [0.85,0.15,0.0] $
    , [0.5, 0.5, 0.0] $
    , [0.8, 0.2, 0.0]]
  print, 'No propagation:   ', nrs_get_bayes_prob(exp_prob, fltarr(3) + 1)   
  print, 'With propagation: ', nrs_get_bayes_prob(exp_prob, fltarr(3) + 1, /prop)
end