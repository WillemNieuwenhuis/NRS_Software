;+
; :description:
;    Read point locations from a CSV table. The module tries to identify potential
;    coordinate fields: it looks for fields starting with 'lat', 'lon', 'x', and 'y' (case insensitiv).
;    If 'lat' and 'lon' or 'x' and 'y' are found together the coordinate parameters X and Y
;    are initalized to the values. If both pairs appear then  'lat' and 'lon' take preference.
;    If no coordinates are found the X and Y parameter are empty arrays
;    
;    The data keyword simply contains the entire table upon finishing, numerical string values
;    will have been converted to actual numbers.
;    
;    Date and time fields will be copied as-is (strings) from the file.
;
; :params:
;    table : in
;      The name of the CSV file
;    x : out
;      The X or Longitude coordinates
;    y : out
;      The Y or Latitude coordinates
;
; :keywords:
;    data : out, optional
;      The entire table parsed according to type
;    nr_cols : out, optional
;      Return the number of fields in the table
;    nr_recs : out, optional
;      Return the number of records found
;    header : out, optional
;      Return the header line
;    valid : out, optional
;      Indicates if the table could be read succesfully
;    hint_geo : out, optional
;      Indicates if any coordinates found are geographic (hint_geo = 1) or metric (hint_geo = 0).
;    attr_fields : in, optional
;      String array with the names of the fields to extract data from
;    attr_value : out
;      Array with attribute data for all fields in attr_fields. It contains only data for
;      existing fields. See also attr_valid.
;      All data is returned either as string or as number.
;    attr_valid: out
;      Indicates which of the fields in attr_fields could be found and are extracted in attr_value 
;
; :author: nieuwenhuis
; 
; :history:
; - august 2013: WN, created
; - august 2015: WN, added attribute data collection
;-
pro nrs_read_points_csv, table, x, y, data = asc, valid = valid  $
                       , attr_fields = attr_fields, attr_value = attr, attr_valid = attr_valid $
                       , hint_geo = hint_geo $
                       , nr_cols = field_count, nr_recs = nrrec, header = header
  compile_opt idl2, logical_predicate
  
;  line_data = nrs_read_table(table, col_count = field_count, header = header, valid = valid)
 
  do_attr = keyword_set(attr_fields)
  
  asc = nrs_read_csv(table, header = header, date_sep = '-/', time_sep = ':')
  field_count = n_tags(asc)
 
  ; expect at least 2 columns
  valid = field_count ge 2
  if ~valid then return
  
  nrrec = n_elements(asc.(0))
  
  hdr = strlowcase(header)
  typ = size(hdr, /type)
  if typ eq 7  && n_elements(hdr) eq 1 then begin 
    parts = strsplit(hdr, ',', /extract)
  endif else begin
    parts = hdr
  endelse
  lat_ix = where(strmid(parts, 0, 3) eq 'lat', cnt_lat)
  lon_ix = where(strmid(parts, 0, 3) eq 'lon', cnt_lon)
  x_ix = where(strmid(parts, 0, 1) eq 'x', cnt_x)
  y_ix = where(strmid(parts, 0, 1) eq 'y', cnt_y)
  
  if do_attr then begin
    atix = intarr(n_elements(attr_fields))
    for i = 0, n_elements(attr_fields) - 1 do begin
      atix[i] = where(strlowcase(attr_fields[i]) eq parts)
    endfor
    av = where(atix ge 0, val_attr_cnt)
    if val_attr_cnt gt 0 then begin
      atix = atix[av]
      attr = strarr(val_attr_cnt, nrrec)
      attr_valid = bytarr(n_elements(attr_fields))
      attr_valid[av] = 1
      for i = 0,  val_attr_cnt - 1 do begin
        attr[i, *] = transpose(asc.(atix[i]))
      endfor
    endif
  endif
  
  x = []
  y = []
  hint_geo = (cnt_lat + cnt_lon) eq 2
  if cnt_lat le 0 || cnt_lon le 0 then begin
    if x_ix le 0 || y_ix le 0 then begin
      return
    endif
    lat_ix = y_ix
    lon_ix = x_ix
    cnt_lat = cnt_y
    cnt_lon = cnt_x
  endif
  ix = lindgen(field_count)
  count = field_count

  if cnt_lon ne 0 && cnt_lat ne 0 then begin
    x = asc.(lon_ix)
    y = asc.(lat_ix)
  endif
  
  maxx = max(x, min = minx)
  maxy = max(y, min = miny)
  hint_geo = hint_geo $
             && ((abs(maxx) le 360.0 && abs(minx) le 360.0) $
               && (abs(maxy) le 360.0 && abs(miny) le 360.0))

end
