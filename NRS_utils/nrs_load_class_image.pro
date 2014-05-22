;+
; :description:
;    Load a classified image
;
; :params:
;    fid
;      The ID of the already opened image
;
; :keywords:
;    cldata : out
;      The class data
;    cnames : out
;      The class names if available
;    num_classes : out
;      The number of different classes
;    mi : out
;      The coordinate system, -1 if undefined
;
; :author: Willem Nieuwenhuis
;-
pro nrs_load_class_image, fid, cldata = cldata, cnames = cnames, num_classes = nrclass, mi = mi_ref
  compile_opt idl2, logical_predicate
  
  if fid eq -1 then cldata = 0
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, class_names = cnames, num_classes = nrclass
  mi_ref = envi_get_map_info(fid = fid, undefined = undef_csy)
  if undef_csy eq 1 then mi_ref = -1
  
  ; Load classified image
  cldata = envi_get_data(fid = fid, dims = dims, pos = 0)
  
  ; handle presence of 'Unclassified' in main classified map
  if nrclass eq 0 then begin
    ix = where(cldata lt 32000, count)
    nrclass = max(cldata[ix])
  endif else begin
    has_uncl = where(strlowcase(cnames) eq 'unclassified')
    if has_uncl ne -1 then begin
      nrclass--
      cldata -= 1
    endif
  endelse
end

