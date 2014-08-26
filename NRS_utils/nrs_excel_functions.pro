;+
; :description:
;    Convert an excel column index to an IDL index
;
; :returns:
;   A zero based IDL index
;
; :params:
;    colstr : in, required
;      The excel column index, such as "AK"
;
; :author: nieuwenhuis
;-
function nrs_excel_colix_as_num, colstr
  compile_opt idl2, logical_predicate, hidden
  
  colnum = byte(strupcase(colstr)) - (byte('A'))[0] + 1
  colnum[-1] -= 1
  factor = 26 ^ (n_elements(colnum) - indgen(n_elements(colnum)) - 1)
  
  return, long(total(colnum * factor))
end

;+
; :description:
;    Convert an IDL index value to an excel column index (for example: "AK")
;
; :returns:
;    Excel column index
;
; :params:
;    colind : in, required
;      The zero based IDL
;
; :author: nieuwenhuis
;-
function nrs_colix_to_excel_range, colind
  compile_opt idl2, logical_predicate, hidden
  
  max_dig = 5
  factor = 26L ^ indgen(max_dig)
  facacc = long(total(factor, /cum)) - 1
  ix = where(colind ge facacc, nr_dig)
  
  digits = []
  num = colind
  for d = nr_dig, 1, -1 do begin
    dig = num / factor[d - 1]
    digits = [digits, dig]
    num -= factor[d - 1] * dig
  endfor
  digits[-1] += 1
  digits--
  
  return, string(byte(digits + (byte('A'))[0]))
end

;+
; :description:
;    Convert an excel range into an IDL index (zero based) or vice versa
;
; :params:
;    range : in, required
;      The range / index to be converted. If it is an excel range it must contain absolute ranges,
;      for example: "$A$1:$AK$62"; thus including the '$' for absolute addressing.
;
;      In case of IDL range, the parameter is a 4 element array: [col_min, col_max, row_min, row_max]
;
;      The IDL indices are zero-based.
;
;      The function will determin the direction of the conversion automatically.
;
; :author: nieuwenhuis
;
; :history:
; - 14 Aug 2014, created
;-
function nrs_excel_range, range
  compile_opt idl2, logical_predicate
  
  to_index = n_elements(range) eq 1
  to_excel = n_elements(range) eq 4
  if ~to_excel && ~to_index then begin
    return, []
  endif
  
  if to_index then begin
    split = stregex(range, '.([A-Z]+).([0-9]+):.([A-Z]+).([0-9]+)', /subexpr, /extract)
    if n_elements(split) ne 5 then return, []
    
    col_min = nrs_excel_colix_as_num(split[1])
    row_min = long(split[2]) - 1
    col_max = nrs_excel_colix_as_num(split[3])
    row_max = long(split[4]) - 1
    
    return, [col_min, col_max, row_min, row_max]
  endif $
  else begin
    cx = [nrs_colix_to_excel_range(range[0]) , nrs_colix_to_excel_range(range[1])]
    rx = string(range[2:3] + 1, format = '(i0)')
    return, '$' + cx[0] + '$' + rx[0] + ':$' + cx[1] + '$' + rx[1]
    endelse
end

pro nrs_read_excel_data, xls_table, skip_rows, skip_cols
  compile_opt idl2, logical_predicate
  
  excel = obj_new("IDLcomIDispatch$PROGID$Excel_Application")
  
  excel->getproperty, Workbooks = workbook
  
  xlsdata = workbook->open(xls_table)
  
  xlsdata->getproperty, sheets = sheets
  
  sheets->getproperty, count = nr_sheets
  
  sheet_names = strarr(nr_sheets)
  for i = 1, nr_sheets do begin
    sheets->getproperty, i, item = cur_sheet
    
    cur_sheet->getproperty, name = curname
    cur_sheet->getproperty, usedrange = range
    range->getproperty, addresslocal = al ; a string that looks like: "$A$1:$AK$62"
    
    sheet_names[i - 1] = curname
    
    obj_destroy, cur_sheet
  endfor
  
  ; now get values
  cur->getproperty, "B3:AK62", range=cells
  cells->getproperty, value=myValues
  
  dims = size(myvalues, /dim)
  print, 'value dimensions ', dims
  myValues = transpose(reform(temporary(myValues), dims[1], dims[0]))
  ;  print, myvalues, format = '(36(i0," "))'
  
  ; cleanup, close excel object
  bd->setproperty, saved=1
  wb->close
  oexcel->quit
  obj_destroy, [cells, cur, range, sh, bd, wb, excel]
end

pro excel_test
  compile_opt idl2, hidden, logical_predicate
  
  excel = obj_new("IDLcomIDispatch$PROGID$Excel_Application")
  
  excel->getproperty, Workbooks = wb
  
  bd=wb->open("E:\NRS\Kees-Andrew\Insurance payment\tables_eth.xlsx")
  
  bd->getproperty,sheets=sh
  
  sh->getproperty, count = nr_sh
  
  for i=1, nr_sh do begin
    sh->getproperty,i,Item=cur
    
    cur->getproperty,Name=curname
    if i lt nr_sh then obj_destroy, cur
    
    print,curname
  endfor
  
  cur->getproperty, usedrange = range
  ;  range->getproperty, value=myValues
  range->getproperty, height = hgt
  range->getproperty, width = wdt
  range->getproperty, addresslocal = al
  range->getproperty, address = al_rng
  
  print, hgt, wdt
  print,al
  print,al_rng
  
  ;  print, size(myvalues, /dim)
  
  ; now get values
  cur->getproperty, "B3:AK62", range=cells
  cells->getproperty, value=myValues
  
  dims = size(myvalues, /dim)
  print, 'value dimensions ', dims
  myValues = transpose(reform(temporary(myValues), dims[1], dims[0]))
  ;  print, myvalues, format = '(36(i0," "))'
  
  ; cleanup, close excel object
  bd->setproperty, saved=1
  wb->close
  oexcel->quit
  obj_destroy, [cells, cur, range, sh, bd, wb, excel]
end
