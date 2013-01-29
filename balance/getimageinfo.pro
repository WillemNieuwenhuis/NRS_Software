;+
; :Description:
;    Get metadata from the image.
;
; :Params:
;    imageid : in, required
;      The ENVI handle of the image
;
; :Author: nieuwenhuis
; :History: Jan 9, 2013
;-
function getImageInfo, imageid
  compile_opt idl2

  envi_file_query, imageid, ns = ns, nl = nl, nb = nb, dims = dims
  bandlist = indgen(nb)
  envi_convert_file_coordinates, imageid, [0, ns - 1], [0, nl - 1], xmap, ymap, /to_map
  
  return, { $
            nbrows:nl $
          , nbcolumns : ns $
          , nbbands : nb $
          , bands : bandlist $
          , dimensions : dims $
          , mapx : xmap $
          , mapy : ymap $
        }
end
