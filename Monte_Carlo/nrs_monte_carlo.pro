function nrs_carbon_allometric_shorea, dbh
  compile_opt idl2, logical_predicate
  
  res = 0.47 * exp(-2.193 + 2.371 * alog(dbh)) 
  return, res
end

function nrs_carbon_allometric_other, dbh
  compile_opt idl2, logical_predicate
  
  res = 0.47 * exp(-1.201 + 2.196 * alog(dbh)) 
  return, res
end

pro nrs_read_carbon, table, carbon = carbon, cpa = cpa, height = height
  compile_opt idl2, logical_predicate
  
  asc = nrs_read_csv(table, header = header)
  sz = size(asc.field1, /dim)
  if n_elements(sz) eq 0 then return
  if sz[0] le 0 then return

  ; table header contains: FID, PlotID, TreeID, DBH, CPA, Height
  hdr = strlowcase(header)
  carbon_ix = where(hdr eq 'carbon', caebon_cnt)
  cpa_ix = where(hdr eq 'cpa', cpa_cnt)
  height_ix = where(hdr eq 'height', height_cnt)
  
  if cpa_cnt + height_cnt eq 0 then begin
    void = dialog_message('No data found', /error)
    return
  endif

  carbon = asc.(carbon_ix)
  cpa = asc.(cpa_ix)
  height = asc.(height_ix)
  nr_obs = n_elements(carbon)
end

pro nrs_monte_carlo, species_table, outname = outname $
                   , prog_obj = progressBar, cancelled = cancelled $
                   , n_iter $
                   , save_all_runs = save_all_runs $
                   , es_s, es_a $         ; number of samples (in %, for Segmentation, accuracy)
                   , ec_s, ecw_s $        ; Ecarbon (correct, wrongly classified)
                   , ep_well, ep_poor $   ; Ecpa    (correct, wrongly classified)
                   , eh_well, eh_poor     ; Eheight (correct, wrongly classified)
  compile_opt idl2, logical_predicate
  
  cancelled = 0
  nrs_set_progress_property, prog_obj, /start, title = 'Monte Carlo simulation for carbon'
  
  nrs_read_carbon, species_table, carbon = carbon, cpa = cpa, height = height
  
  n_shorea = n_elements(carbon)
  es_s *= n_shorea / 100.0
  es_a *= n_shorea / 100.0
  carb_est = fltarr(n_shorea)
  cpa_est = fltarr(n_shorea)
  height_est = fltarr(n_shorea)
  regres_list = fltarr(3, n_iter) ; const, coef_cpa, coef_height
  rmse = fltarr(n_iter)
  for iter = 0, n_iter - 1 do begin
    if iter mod 10 eq 0 then $
      if nrs_update_progress(prog_obj, iter, n_iter, cancelled = cancelled) then return
    
    ; Step 1, 2, 3: add random error to carbon, cpa and height
    carbon_err = carbon + (randomu(seed, n_shorea) * 2.0 - 1.0) * ec_s
    carbon_w_err = carbon + (randomu(seed, n_shorea) * 2.0 - 1.0) * ecw_s
    cpa_well = cpa + (randomu(seed, n_shorea) * 2.0 - 1.0) * ep_well
    cpa_poor = cpa + (randomu(seed, n_shorea) * 2.0 - 1.0) * ep_poor
    height_well = height + (randomu(seed, n_shorea) * 2.0 - 1.0) * eh_well
    height_poor = height + (randomu(seed, n_shorea) * 2.0 - 1.0) * eh_poor
    
    ; Step 5: randomly select combination
    ; initialization
    carb_est = carbon_err
    cpa_est = cpa_well
    height_est = height_well
    ; randomly select S' (segmentation)
    ix_seg = fix(randomu(seed, es_s) * n_shorea)
    cpa_est[ix_seg] = cpa_poor[ix_seg]
    height_est[ix_seg] = height_poor[ix_seg]
    ; randomly select A' (classifcation accuracy)
    ix_acc = fix(randomu(seed, es_a) * n_shorea)
    carb_est[ix_acc] = carbon_w_err[ix_acc]

    ; Step 6: Generate the regression model
    regres = regress([transpose(cpa_est), transpose(height_est)], carb_est, const = const)
    regres_list[*, iter] = [const, regres[0 : 1]]
    ; Step 7: Calculate the carbon based on the model
    estimation = const + regres[0] * cpa + regres[1] * height
    rmse[iter] = sqrt(total((estimation - carbon) ^ 2) / n_shorea)
    if keyword_set(save_all_runs) then begin
      header = ['cpa', 'height', 'carbon', 'cpa_err', 'height_err', 'carbon_err', 'carbon_estim', 'C', 'a1', 'b1', 'rmse']
      outdata = create_struct('cpa', cpa $
                            , 'height', height $
                            , 'carbon', carbon $
                            , 'cpa_err', cpa_est $
                            , 'height_err', height_est $
                            , 'carbon_err', carb_est $
                            , 'carbon_estim', estimation $
                            , 'C', fltarr(n_shorea) + const $
                            , 'a1', fltarr(n_shorea) + regres[0] $  ; cpa
                            , 'b1', fltarr(n_shorea) + regres[1] $  ; height
                            , 'rmse', fltarr(n_shorea) + rmse[0] $  ; rmse
                          )
                          
      iter_str = string(iter, format = '(i05)')
      on = getOutname(species_table, postfix = '_regr_' + iter_str, ext = '.csv')
      write_csv, on, header = header, outdata
    endif
  endfor
  
  header = ['iter', 'C', 'a1', 'b1', 'rmse']
  outdata = create_struct('iter', indgen(n_iter) + 1 $
                        , 'C', regres_list[0, *] $
                        , 'a1', regres_list[2, *] $ ; Height
                        , 'b1', regres_list[1, *] $ ; CPA
                        , 'rmse', rmse $
                      )
                      
  if n_elements(outname) eq 0 then outname = getOutname(species_table, postfix = '_regr', ext = '.csv')
  write_csv, outname, header = header, outdata
  
end
