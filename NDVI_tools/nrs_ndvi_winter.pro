;In one year, for every pixel: 
;1. For layers NO. 29 to NO.33 (11th Oct. to 21th Nov.) [1-based;wn]
;1.1 If the pixel values are in the range of 0 to 0.98
;1.2 Choose two maximum values, M1 and M2,
;1.3 Then calculate initial Winterndvi=sqrt(M1*M2)
;1.4 Then final WinterNDVI=0.3+0.5*(1-EXP (-5.7Winterndvi))
;2. The last step is replacing pixel values of images NO.29-NO.36 and NO.1 to NO.9 with this WinterNDVI.
;3. Then move to next pixel.
;4. Then move to next year

;+
; :Description:
;    Calculate winter NDVI and replace winter months NDVI values with winter NDVI. There
;    are two scenarios:
;    <ol>
;    <li> replace all winter months with the new NDVI (change_winter = 1)
;    <li> replace all months with the max of the new NDVI and the original NDVI for that month(change_all = 1)
;    </ol> 
;    Additionally the summer NDVI can be smoothed
;
; :Params:
;    image_name : in, required
;      Name of the input NDVI image
;    outname : out, required
;      Name of the output image
;
; :Keywords:
;    change_winter : in, optional
;      Replace only winter months with the calculated winter NDVI; takes preference over 'change_all'
;    change_all : in, optional
;      Replace all month NDVI values with the maximum of the winter NDVI and the NDVI value for 
;      each month; is overruled by 'change_winter'
;    adjust_summer : in, optional
;      If specified and non-zero: replace the summer NDVI with the average of surrounding NDVI values
;      that are larger than 0.3. This adjustment is done after any winter NDVI adjustment 
;
; :Author: nieuwenhuis
;-
pro nrs_ndvi_winter, image_name, outname $
                    , change_winter = change_winter, change_all = change_all $
                    , adjust_summer = adjust_summer
  compile_opt idl2, logical_predicate
  
  envi_open_file, image_name, r_fid = fid, /no_realize, /no_interactive_query
  
  if fid eq -1 then return
  
  doPeriod = ~((n_elements(change_winter) eq 0) || (change_winter eq 0)) 
  doAll = ~((n_elements(change_all) eq 0) || (change_all eq 0))
  if doPeriod then doAll = 0
;  if (doPeriod and doAll) eq 0 then doPeriod = 1
  doWinter = doPeriod || doAll
  
  adjust_summer = keyword_set(adjust_summer)
  
  img_per_year = 36
  
  envi_file_query, fid, nb = nb, ns = ns, nl = nl $
                 , data_type = dt, dims = dims $
                 , xstart = xs, ystart = ys $
                 , bnames = bnames, data_ignore_value = undef
  mi = envi_get_map_info(fid = fid, undefined = csy_undef)
  if csy_undef eq 1 then delvar, mi
  
  nr_years = fix(nb / img_per_year)
 
  openw, unit, outname, /get_lun
  holder = assoc(unit, make_array(ns, nb, type = dt))  ; bil

  progressBar = Obj_New("PROGRESSBAR", background = 'white', color = 'green', ysize = 15, title = "Calculating winter NDVI")
  progressBar -> Start
  
  catch, stat
  if stat ne 0 then begin
    ; cleanup if some error
    close, unit  ; close assoc
    free_lun, unit
    file_delete, outname, /noexpand_path, /allow_nonexistent, /quiet
    progressBar -> Destroy
    catch, /cancel
    ans = dialog_message(!error_state.msg, title = 'Error', /error)
    return
  endif
  
  repl_ar = [indgen(9), indgen(8) + 28] ; define winter period
  autumn_ix = indgen(5) + 28
  summer_ix = indgen(19) + 9
  tall = nl
  for line = 0, nl - 1 do begin
    cube = envi_get_slice(fid = fid, /bil, line = line, xs = 0, xe = ns - 1)
    tpos = line
    progressBar -> Update, 100.0 * (tpos + 1) / tall, text = 'Progress: ' + string(tpos + 1, format = '(i0)') + ' of ' + string(tall, format = '(i0)')
    cancelled = progressBar -> CheckCancel()
    if cancelled eq 1 then begin
      progressBar -> Destroy
      ans = dialog_message('Calculation interrupted by user', title = 'Information', /information)
      return
    endif
    
    for y = 0, nr_years - 1 do begin
      sb = img_per_year * y
      eb = sb + img_per_year - 1
      for p = 0, ns - 1 do begin
        pixar = cube[p, sb : eb]
        autumn = pixar[autumn_ix]
        ix = where(autumn ge -0.1 && autumn le 0.98, count)
        
        ; calculate winter NDVI
        winterndvi = 0.3
        if count ge 2 then begin
          valid = pixar[ix]
          six = valid[sort(valid)]
          m1 = six[count - 1]
          m2 = six[count - 2]
          if m1 lt 0 then m1 = 0
          if m2 lt 0 then m2 = 0
          winterndvi = sqrt(m1 * m2)
          winterndvi = 0.3 + 0.5 * (1 - exp(-5.7 * winterndvi))
        endif
        
        ; Now replace winter NDVI values if needed
        if count ge 1 then begin
          sf_ix = where(pixar[summer_ix] lt 0.3, cnt_summ)  ; get all summer NDVI lower than 0.3
          if doWinter then begin
            if doPeriod then begin
              ; only adjust winter time
              pixar[repl_ar] = winterndvi
            endif else begin
              ; replace all NDVI lower than 0.3
              ind = where(pixar le 0.3, count)
              if count gt 0 then pixar[ind] = winterndvi
            endelse
          endif
          
          ; Adjust summer NDVI if there are spikes
          if adjust_summer then begin
            if cnt_summ gt 0 then begin
              for s = 0, cnt_summ - 1 do begin
                st = sf_ix[s] + summer_ix[0]  ; get index of NDVI value to replace
                sum = 0.0
                sum_cnt = 0
                if pixar[st - 1] ge 0.3 then begin
                  sum += pixar[st - 1]
                  sum_cnt++
                endif
                if pixar[st + 1] ge 0.3 then begin
                  sum += pixar[st + 1]
                  sum_cnt++
                endif
                pixar[st] = sum / sum_cnt
              endfor
            endif
          endif
          
          ; store the adjusted NDVI
          cube[p, sb : eb] = pixar
        endif
      endfor
    endfor
    holder[line] = cube  ; bil storage
  endfor

  envi_setup_head, fname = outname $
          , data_type = dt $
          , /write $
          , interleave = 1 $  ; BIL
          , nb = nb, nl = nl, ns = ns $
          , bnames = bnames $
          , map_info = mi $
          , xstart = xs, ystart = ys $
          , data_ignore_value = undef

  close, unit
  free_lun, unit  ; close assoc

  progressBar -> Destroy
end
