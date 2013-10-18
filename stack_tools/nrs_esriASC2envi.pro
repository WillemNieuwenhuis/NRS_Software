function get_value_by_key, kv, key, datatype
  compile_opt idl2, logical_predicate

  kix = where(key eq kv.key, bytecnt)
  if bytecnt eq 0 then return, []
  
  case datatype of
    3: return, long((kv.value)[kix])
    4: return, float((kv.value)[kix])
    7: return, (kv.value)[kix]
  endcase
end

function parse_keys, meta
  compile_opt idl2, logical_predicate

  kv = []
  for i = 0, n_elements(meta) - 1 do begin
    parts = strsplit(meta[i], /extract)
    if n_elements(parts) eq 2 then begin
      pair = {key : strtrim(parts[0], 2), value : strtrim(parts[1], 2)}
      kv = [kv, pair]
    endif
  endfor
  return, kv
end

;+
; :description:
;    Convert an ascii rasterfile into a binary file (float).
;    The values are read from the ASCII file, then converted
;    to floating point number and written as binary values.
;    The output filename use the same name as the input, with
;    a new extension (.dat)
;
; :params:
;    filename : in
;     The input filename of the ASCII raster
;
; :author: nieuwenhuis
; 
; :history:
;   17 oct 2013: nieuwenhuis, created
;-
pro nrs_convert_ASCII2bin, filename, skiplines = skiplines
  compile_opt idl2, logical_predicate
  
  if ~(file_info(filename)).exists then return
  
  if n_elements(skiplines) eq 0 then skiplines = 0
  outname = getOutname(filename, postfix = '', ext = '.dat')
  openr, unit, filename, /get_lun
  openw, out_unit, outname, /get_lun
  lx = 0
  line = ''
  while ~eof(unit) do begin
    readf, unit, line
    if lx lt skiplines then begin
      lx++
      continue
    endif
    parts = strsplit(line, /extract)
    data = float(parts)
    writeu, out_unit, data 
  endwhile
  close, unit
  close, out_unit
  free_lun, unit
  free_lun, out_unit
  
end

;meta = [$
;  'ncols 813', $
;  'nrows 670', $
;  'xllcorner 112.925', $
;  'yllcorner -43.575', $
;  'cellsize 0.05', $
;  'nodata_value -9999', $
;  'byteorder lsbfirst' ]
;+
; :description:
;    Read an ESRI binary raster (.FLT) with separate header file (.HDR)
;
; :params:
;    filename : in
;      The name of either the binary or the header file
;
; :author: nieuwenhuis
; 
; :history:
;   17 oct 2013 : nieuwenhuis, created
;-
pro nrs_esriSC2envi, filename
  compile_opt idl2, logical_predicate

  if strlen(strtrim(filename, 2)) eq 0 then begin
    void = dialog_message('No filename specified', /error)
    return
  endif
  
  ext = nrs_get_file_extension(filename)
  usesHeader = ext eq '.flt' || ext eq '.hdr'
  binfile = getOutname(filename, postfix = '', ext ='.flt')
  hdrfile = getOutname(filename, postfix = '', ext ='.hdr')
  ascfile = getOutname(filename, postfix = '', ext ='.asc')
  grdfile = getOutname(filename, postfix = '', ext ='.grd')
  fi_bin = file_info(binfile)
  fi_hdr = file_info(hdrfile)
  fi_asc = file_info(ascfile)
  fi_grd = file_info(grdfile)
  isBinary = usesHeader && (fi_bin.exists && fi_hdr.exists)
  isASCII = ~usesHeader && (file_info(filename)).exists
  
  if isASCII then hdrfile = ascfile
  meta = nrs_read_listfile(hdrfile, lines = usesHeader ? 7 : 6)
  kv = parse_keys(meta)
  
  ns = get_value_by_key(kv, 'ncols', 3) ; 3 == long
  nl = get_value_by_key(kv, 'nrows', 3)
  x_tie_c = get_value_by_key(kv, 'xllcenter', 4)  ; 4 == float
  x_tie_a = get_value_by_key(kv, 'xllcorner', 4)
  y_tie_c = get_value_by_key(kv, 'yllcenter', 4)
  y_tie_a = get_value_by_key(kv, 'yllcorner', 4)
  pixsize = get_value_by_key(kv, 'cellsize', 4)
  undef = get_value_by_key(kv, 'nodata_value', 4)
  byte_order = get_value_by_key(kv, 'byteorder', 7) eq 'msbfirst' ? 1 : 0  ; 7 == string
  okDims = (n_elements(ns) eq 1) && (n_elements(nl) eq 1)
  okCenter = (n_elements(x_tie_c) eq 1) && (n_elements(y_tie_c) eq 1)
  okArea = (n_elements(x_tie_a) eq 1) && (n_elements(y_tie_a) eq 1)
  okPixsize = n_elements(pixsize eq 1)
  if ~okDims || ~(okCenter || okArea) || ~okPixsize then begin
    void = dialog_message('Could not recognize header, probably not an ESRI gridfile', /error)
    return
  endif

  outname = filename
  if isBinary then begin
    ; make a backup of the original header file
    bkfile = getoutname(hdrfile, postfix = '', ext = '.bk')
    nrs_write_listfile, bkfile, meta
    
    outname = binfile
  endif
  
  if isASCII then nrs_convert_ASCII2bin, filename, skiplines = n_elements(kv)

  if okCenter then begin
    pix_x = 0.5
    pix_y = 0.5
    top = y_tie_c + pixsize * (nl - 1.0)
    left = x_tie_c
  endif
  if okArea then begin
    pix_x = 0.0
    pix_y = 0.0
    top = y_tie_a + pixsize * nl
    left = x_tie_a
  endif
  mi = envi_map_info_create(/geographic, mc = [pix_x, pix_y, left, top], ps = [pixsize, pixsize])   
  envi_setup_head, fname = outname $
          , data_type = 4 $   ; float
          , byte_order = byte_order $
          , /write $
          , nb = 1 $
          , nl = nl, ns = ns $
          , bnames = [file_basename(outname)] $
          , map_info = mi $
          , interleave = 0 $
          , data_ignore_value = undef
end
