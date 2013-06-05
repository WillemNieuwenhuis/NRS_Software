;+
; :description:
;    Load a CSV formatted table into memory. The table is assumed to have one header line
;
; :return:
;   The data of the table
;
; :params:
;    table : in, required
;      Fully specified ilename of the table 
;
; :keywords:
;    col_count : out
;      The number of columns detected
;    header : out
;      The header line of the table
;    valid : out
;      Indicate whether the table could be found (1) or not (0)
;
; :author: nieuwenhuis
;-
function nrs_read_table, table, col_count = field_count, header = header, valid = valid 
  compile_opt idl2, logical_predicate
  
  valid = 0
  if strlen(table) eq 0 then return, []
  
  valid = 1
  ht_data = read_ascii(table $
    , header = header $
    , delimiter = ',' $
    , data_start = 1 $
    , record_start = 0, count = count)

  field_count = n_elements(ht_data.(0)) / count
  header = header

  return, ht_data.(0)
end

