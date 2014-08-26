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
;    has_unclassified : out
;      Return TRUE if the class image has a class <unclassified>, FALSE otherwise
;    class_adjust : in, optional, default = yes
;      If set will decrement all class numbers in case the <unclassified> class is defined.
;      This class is always zero, so if the adjustment is activated all classes will be
;      decremented and the <unclassified> is not considered separately.
;
; :author: Willem Nieuwenhuis
;-
pro nrs_load_class_image, fid, cldata = cldata, cnames = cnames, num_classes = nrclass $
                        , mi = mi_ref $
                        , has_unclassified = has_unclassified $
                        , class_adjust = class_adjust
  compile_opt idl2, logical_predicate
  
  if fid eq -1 then cldata = 0
  
  envi_file_query, fid, dims = dims, ns = ns, nl = nl, class_names = cnames, num_classes = nrclass
  mi_ref = envi_get_map_info(fid = fid, undefined = undef_csy)
  if undef_csy eq 1 then mi_ref = -1
  
  ; Load classified image
  cldata = envi_get_data(fid = fid, dims = dims, pos = 0)
  
  ; handle presence of 'Unclassified' in main classified map
  has_unclassified = (where(strlowcase(cnames) eq 'unclassified') ne -1)
  if nrclass eq 0 then begin
    ix = where(cldata lt 32000, count)
    nrclass = max(cldata[ix])
  endif else begin
    if keyword_set(class_adjust) then begin
      if has_unclassified then begin
        nrclass--
        cldata -= 1
      endif
    endif
  endelse
end

