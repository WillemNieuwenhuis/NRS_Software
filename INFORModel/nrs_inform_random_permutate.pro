;+
; :description:
;    Randomly generate sets of parameters. For each parameter a range
;    is specified, as well as a distribution (in parms)
;    A parameter set contains one randomly generated for each of the parameters
;
; :params:
;    sample_count: in, required
;      The number of parameter sets to create
;
; :keywords:
;    permutation: out
;      The output parameter sets
;    prog_obj : in, optional
;      A progressBar object to be used to display progress
;    cancelled : out
;      Indicates if the process was interupted by the user in the progressBar
;
; :Author: nieuwenhuis
; 
; :history:
;   - 13 Feb 2015: nieuwenhuis, created
;
;-
pro nrs_inform_random_permutate, sample_count, parms $
            , permutation = permutation $
            , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate
  
end