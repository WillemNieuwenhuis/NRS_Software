;+
; :Description:
;   Select a shapefile and open it, returning details through the keyword parameters
; 
; :Params:
;   filename:  in
;     filename of the shapefile
;     
; :Keywords:
;   myshape: out
;     Reference to the open shapefile object
;   num_ent: out
;     The number of features in the shape file
;   ent_type: out
;     The feature type in the shape file
;   num_attr: out
;     The number of attributes currently in the shapefile
;   attr_info: out
;     Detailed information about all the attributes in the shapefile
;     
;-
pro nrs_openShapefile, filename, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info
  compile_opt idl2, logical_predicate
  
  myshape = OBJ_NEW('IDLffShape', filename)

  ; Get the number of entities and the entity type.
  myshape->IDLffShape::GetProperty, N_ENTITIES = num_ent, $
      ENTITY_TYPE = ent_type, N_ATTRIBUTES = num_attr

  ; Get the info for all attributes (if any).
  if num_attr gt 0 then begin
    myshape->IDLffShape::GetProperty, ATTRIBUTE_INFO = attr_info
  end

end

;+
; :Description:
;   Create a copy of a shapefile, features, projection and attributes
;   and return the new object in <b> newShape</b>
; :Params:
;   inputShape: in
;     Open input shapefile object
;   newShape: out
;     The new shapefile (output)
;     
; :keywords:
;   outputShapeName: in, optional
;     The name of the new shapefile
;-
pro createNewShapefile, inputShape, newShape, newname = outputShapeName
  compile_opt idl2, logical_predicate

  inputShape->IDLffShape::GetProperty, FILENAME = inputName
  
  ; Get the number of entities and the entity type.
  inputShape->IDLffShape::GetProperty, N_ENTITIES = num_ent, $
      ENTITY_TYPE = ent_type, N_ATTRIBUTES = num_attr

  ; Get the info for all attributes (if any).
  if num_attr gt 0 then begin
    inputShape->IDLffShape::GetProperty, ATTRIBUTE_INFO = attr_info
  end

  if n_elements(outputShapeName) eq 0 then $
    outputShapeName = getOutname(inputName, postfix = '_copy')

  ;Create a new shape file
  newshape = OBJ_NEW('IDLffShape', outputShapeName, /UPDATE, ENTITY_TYPE = ent_type)

    ; copy the existing attribute definition to the new shape file
  for i = 0, num_attr - 1 do begin
    newshape->IDLffShape::AddAttribute, attr_info[i].name, $
      attr_info[i].type, $
      attr_info[i].width, $
        PRECISION = attr_info[i].precision
  endfor
  
  copyProjectionFile, inputShape, newshape
end

;+
; :Description:
; Copy the <shape>.prj to the new shape file
; :Params:
;   myshape
;     Open shapefile object, whose projection needs to be copied
;   outputShape
;     Open shapefile object, whose projection needs to be set
; :Author:
;   Willem Nieuwenuis, 2007
;-
pro copyProjectionFile, myshape, outputShape
  compile_opt idl2, logical_predicate

  myshape->IDLffShape::getProperty, FILENAME = infile
  dotloc = strpos(infile, '.', /REVERSE_SEARCH)
  inputProjFile = strmid(infile, 0, dotloc) + '.prj'
  outputShape->IDLffShape::getProperty, FILENAME = outputName
  dotloc = strpos(outputName, '.', /REVERSE_SEARCH)
  outputProjFile = strmid(outputName, 0, dotloc) + '.prj'

  ; does the inout projection file exist?
  if file_test(inputProjFile) ne 1 then return

  file_copy, inputProjFile, outputProjFile, /overwrite
end

;+
; :description:
;   read the point locations from a shape file
;
; :returns:
;   An array of coordinate structures; the structure contains {X, Y, sort_code, shape_id}.
;   
; :params:
;   shape: in
;     Filename of shape file
;     
; :keywords:
;   hint_geo : out
;     indicate if the projection / coordinates is / are geograpic
;
; :author: nieuwenhuis
; :history:
;   - sept 2007: created
;   - jan 2014: function renamed and changed to handle only a single shape file 
;-
function nrs_read_shape_points, shapes, hint_geo = hint_geo
  compile_opt idl2, logical_predicate
  
  coordinate = {Coordinate, $
        X : double(0), $
        Y : double(0), $
        S : double(0), $  ; value used for sorting
        ID  : string('') $
        }

  ; open the input shapefile
  shape = OBJ_NEW('IDLffShape', shapes)

    ; Get the number of entities and the entity type.
  shape->IDLffShape::GetProperty, n_entities = num_ent, $
      entity_type = ent_type, n_attributes = num_attr

  pnt_types = [1, 11, 21]
  ix = where(ent_type eq pnt_types, pt_cnt)
  if pt_cnt eq 0 then begin
    obj_destroy, shape
    void = error_message('Only point features supported')
    return, []
  endif

  coords = replicate(coordinate, num_ent)
  for sh = 0, num_ent - 1 do begin
    feature = shape->IDLffShape::GetEntity(sh, /attributes)
    coords[sh].X = feature.bounds[0]
    coords[sh].Y = feature.bounds[1]
    coords[sh].S = feature.bounds[1] * 10000000 + feature.bounds[0]

    ; Get the (string) ID of the feature
    if num_attr gt 0 then begin
      shape->IDLffShape::GetProperty, ATTRIBUTE_INFO = attr_info
      for at = 0, num_attr - 1 do begin
        if (strupcase(attr_info[at].name) eq 'ID') then begin
          attr = feature.attributes
          sid = (*attr).(at)
          coords[sh].ID = sid
        endif
      endfor
    endif
    
    shape->destroyentity, feature
  endfor

  obj_destroy, shape

  maxx = max(coords.x, min = minx)
  maxy = max(coords.y, min = miny)
  hint_geo = ((abs(maxx) le 360.0 && abs(minx) le 360.0) $
           && (abs(maxy) le 360.0 && abs(miny) le 360.0))

  return, coords
end

;----------
; Cleanup open resources, close all shape objects
pro nrs_close_shapes, shapes = shapes
  compile_opt idl2, logical_predicate

  for i = 0, n_elements(shapes) - 1 do begin
    shapes[i]->idlffshape::close
    obj_destroy, shapes[i]
  endfor
end