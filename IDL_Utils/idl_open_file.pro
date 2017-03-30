;+
; Group of functions to handle ENVI data files. They are meant to
; imitate the functionality of the ENVI functions within IDL to
; avoid the dependency of ENVI. 
;-

;+
; :description:
;    Open an ENVI data file, by reading the header file, and collecting
;    some metadata as well.
;
; :params:
;    image : in, required
;      The name of the data file. The header file is assumed to have
;      the same base name as image with the extension '.hdr'
;    meta : in/out, required
;      The meta data collected from the header file. At this time
;      this is a subset of the tags in the header file
;
; :author:
;   nieuwenhuis
; :history:
;   - 6 Feb 2015: created
;-
pro idl_open_file, image, meta
  compile_opt idl2, logical_predicate
  
  idl_read_header, image, meta = meta
  if meta.data_type eq 0 then begin
    void = dialog_message('Not an ENVI data file', title = 'Fatal error', /error)
    return
  endif
  
  meta.data_bsq = ptr_new(make_array(meta.samples, meta.lines, type = meta.data_type), /no_copy)
  meta.data_bil = ptr_new(make_array(meta.samples, meta.bands, type = meta.data_type), /no_copy)
 
  openr, unit, image, /get_lun, error = error
  
  meta.handle = unit
  
end

;+
; :description:
;    Close the data file, opened with idl_open_file.
;
; :params:
;    meta: in, required
;      The meta data of the data file
;
; :author: 
;   nieuwenhuis
; :history:
;   - 6 Feb 2015: created
;-
pro idl_close_file, meta
  compile_opt idl2, logical_predicate
  
  if ptr_valid(meta.data_bsq) then ptr_free, meta.data_bsq 
  if ptr_valid(meta.data_bil) then ptr_free, meta.data_bil
  close, meta.handle
  free_lun, meta.handle

  meta.handle = -1
end

;+
; :description:
;    Read data layer (bsq)
;
; :returns:
;   0 in case of an error, 1 if the read operation succeeded
;
; :params:
;    band : in, required
;      The band number of the data to read
;    meta : in, required
;      The file descriptor structure
;
; :keywords:
;    data : out, required
;      The named variable for the band data from the file.
;
; :author:
;   nieuwenhuis
; :history:
;   6 Feb 2015: created
;-
function idl_get_data, band, meta, data = data
  compile_opt idl2, logical_predicate

  if n_params() lt 2 then begin
    void = dialog_message('Missing parameters', title = 'Fatal error', /error)
    ; serious error
    return, 0
  endif

  band = band[0]  ; remove any array indirection
  
  if band lt 0 or band ge meta.bands then begin
    ; nothing to do
    return, 0
  endif
  
  bsq_size = meta.lines * meta.samples * meta.bytes_per_pixel
  bilp_size = meta.bands * meta.samples * meta.bytes_per_pixel
  line_size = meta.lines * meta.bytes_per_pixel
  sample_size = meta.bytes_per_pixel
  ldata = make_array(meta.samples, type = meta.data_type, /nozero)
  sdata = make_array(1, type = meta.data_type, /nozero)
  if n_elements(data) eq 0 then data = make_array(meta.samples, meta.lines, meta.data_type, /nozero)
  if meta.interleave eq 0 then begin ; BSQ, so same as requested
    loc = band * bsq_size 
    point_lun, meta.handle, loc
    readu, meta.handle, data, transfer_count = tc
    if meta.lines * meta.samples gt tc then begin
      void = dialog_message('Trying to read past end of file', title = 'Fatal error', /error)
      ; serious error
      return, 0
    endif
  endif else if meta.interleave eq 1 then begin ; BIL
    loc = lindgen(meta.lines) * bilp_size + band * line_size
    for l = 0, meta.lines - 1 do begin
      point_lun, meta.handle, loc[l]
      readu, meta.handle, ldata, transfer_count = tc
      if meta.samples gt tc then begin
        void = dialog_message('Trying to read past end of file', title = 'Fatal error', /error)
        ; serious error
        return, 0
      endif
      data[*, l] = ldata
    endfor
  endif else if meta.interleave eq 2 then begin ; BIP
    loc = lindgen(meta.samples) * bilp_size + band * sample_size
    for l = 0, meta.lines - 1 do begin
      point_lun, meta.handle, loc[l]
      readu, meta.handle, sdata, transfer_count = tc
      if 1 gt tc then begin
        void = dialog_message('Trying to read past end of file', title = 'Fatal error', /error)
        ; serious error
        return, 0
      endif
      data[*, l] = sdata
    endfor
  endif
  return, 1
end

function idl_get_bil_slice, line, meta
  compile_opt idl2, logical_predicate
  
  if n_params() lt 2 then begin
    void = dialog_message('Missing parameters', title = 'Fatal error', /error)
    ; serious error
    return, []
  endif

  line = line[0]  ; remove any array indirection
  
  if line lt 0 or line ge meta.lines then begin
    ; nothing to do
    return, []
  endif

  bsq_size = meta.lines * meta.samples * meta.bytes_per_pixel
  bil_size = meta.bands * meta.samples * meta.bytes_per_pixel
  sample_size = meta.samples * meta.bytes_per_pixel
  if meta.interleave eq 0 then begin ; BSQ input
    data = make_array(meta.samples, meta.lines, type = meta.data_type, /nozero)
    ldata = make_array(meta.samples, type = meta.data_type, /nozero)
    loc = lindgen(meta.bands) * bsq_size + line * sample_size
    for l = 0, meta.bands - 1 do begin
      point_lun, meta.handle, loc[l]
      readu, meta.handle, ldata, transfer_count = tc
      if meta.samples gt tc then begin
        void = dialog_message('Trying to read past end of file', title = 'Fatal error', /error)
        ; serious error
        return, []
      endif
      data[*, l] = ldata
    endfor
  endif else begin ; BIL and BIP input
    data = make_array(meta.samples, meta.bands, type = meta.data_type)
    loc = line * bil_size
    point_lun, meta.handle, loc
    readu, meta.handle, data, transfer_count = tc
    if meta.bands * meta.samples gt tc then begin
      void = dialog_message('Trying to read past end of file', title = 'Fatal error', /error)
      ; serious error
      return, []
    endif
    if meta.interleave eq 2 then begin  ; BIP
      data = reform(data, meta.bands, meta.samples, /overwrite)
      data = transpose(temporary(data))
    endif
  endelse
  
  return, data
end

pro idl_read_header, image, meta = meta
  compile_opt idl2, logical_predicate
  
  ns = -1
  nl = -1
  nb = -1
  dt = 0
  has_undef = 0
  undef = 0
  hdrfile = getOutname(image, postfix = '', ext = '.hdr')
  openr, unit, hdrfile, /get_lun, error = error
  if n_elements(unit) eq 0 then return
  
  line = ''
  keys = ['samples', 'lines', 'bands', 'data type', 'interleave', 'data ignore value']
  inter = ['bsq', 'bil', 'bip']
  while ~eof(unit) do begin
    readf, unit, line
    parts = strlowcase(strtrim(strsplit(line, '=', /extract), 2))
    ix = where(parts[0] eq keys, cnt)
    if cnt eq 0 then continue
    case ix of
      0 : ns = long(parts[1])
      1 : nl = long(parts[1])
      2 : nb = long(parts[1])
      3 : dt = fix(parts[1])
      4 : interleave = where(parts[1] eq inter, icnt)
      5 : begin
            has_undef = 1
            if dt ne 0 then $
              undef = fix(parts[1], type = dt)
          end
    endcase
  endwhile

  close, unit
  free_lun, unit

  bpt = [0, 1, 2, 4, 4, 8, 8, 0, 0, 16, 0, 0, 2, 4, 8, 8]

  meta = {metadata2, filename:image, samples:ns, lines:nl, bands:nb $
                  , data_type:dt, interleave:interleave[0] $
                  , bytes_per_pixel:bpt[dt] $
                  , has_undef:has_undef $
                  , data_ignore:undef $
                  , data_bsq:ptr_new() $
                  , data_bil:ptr_new() $
                  , handle:-1}
  
end

; Test routines
; -------------
;pro ttt, lijst = lijst
;  if n_elements(lijst) eq 0 then $
;    lst = nrs_find_images('e:\nrs', 'hdr', extension = 'hdr' , case_sens = 0) $
;  else lst = lijst
;  
;  openw, unit, 'e:\temp\all_envi_hdr.txt', /get_lun
;  printf, unit, 'dir, name, nb, ns, nl, data_type, interleave, undef'
;  foreach name, lst do begin
;    idl_read_header, name, nb = nb, ns = ns, nl = nl, data_type = dt, undef = undef, interleave = interleave
;    if n_elements(nb) gt 0 then nbs = string(nb, format = '(i0)') 
;    if n_elements(ns) gt 0 then nss = string(ns, format = '(i0)')
;    if n_elements(nl) gt 0 then nls = string(nl, format = '(i0)')
;    if n_elements(dt) gt 0 then nds = string(dt, format = '(i0)')
;    if n_elements(interleave) gt 0 then ils = string(interleave, format = '(i0)')
;    if n_elements(undef) eq 0 then undef = 'unknown' else undef = string(undef)
;    printf, unit, strjoin([file_dirname(name), file_basename(name), nbs, nss, nls, nds, ils, undef], ',')  
;  endforeach
;  close, unit
;  free_lun, unit
;end
;
;pro tttl, lijstfile
;  openr, unit, lijstfile, /get_lun
;  
;  ar = []
;  line = ''
;  while ~eof(unit) do begin
;    readf, unit, line
;    if strlen(strtrim(line, 2)) gt 0 then ar = [ar, line] 
;  endwhile
;  
;  close, unit
;  free_lun, unit
;  
;  if n_elements(ar) gt 0 then ttt, lijst = ar
;end
;
;
;pro ttt
;tic
;dd=idl_get_bil_slice(80, meta=meta)
;toc
;tic
;dd=idl_get_bil_slice(81, meta=meta)
;toc
;tic
;dd2=envi_get_slice(fid=fid,line=80,xs=0,xe=358)
;toc
;end
