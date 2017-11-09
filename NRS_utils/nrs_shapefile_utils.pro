;+
; :Description:
;   Select a shapefile and open it, returning details through the keyword parameters
; 
; :Params:
;   filename:  in
;     filename of the shapefile
;   num_ent: out
;     The number of features in the shape file
;   ent_type: out
;     The feature type in the shape file
;   num_attr: out
;     The number of attributes currently in the shapefile
;   attr_info: out
;     Detailed information about all the attributes in the shapefile
;     
; :Keywords:
;   myshape: out
;     Reference to the open shapefile object
;   table_only : in, optional, default = no
;     If set only load the DBF file 
;   update: in, optional, default = no
;     If set allows updating of the shapefile
;     
;-
pro nrs_openShapefile, filename, shape_obj = myshape, num_ent, ent_type, num_attr, attr_info $
                     , update = update, table_only = table_only
  compile_opt idl2, logical_predicate
  
  update = keyword_set(update)
  
  myshape = OBJ_NEW('IDLffShape', filename, update = update, dbf_only = table_only)

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

pro nrs_read_shape_attributes, shapefile, att_names = att_names
  compile_opt idl2, logical_predicate

  if strlowcase(nrs_get_file_extension(shapefile)) ne '.shp' then begin
;    void = error_message('No shapefile specified', traceback = 0)
    return
  endif

  ; open the input shapefile
  shape = obj_new('IDLffShape', shapefile)

  ; Get the number of entities and the entity type.
  shape->idlffshape::getproperty, n_entities = num_ent, $
    entity_type = ent_type, n_attributes = num_attr

  att_names = []
  if num_attr gt 0 then begin
    shape->idlffshape::getproperty, attribute_info = attr_info
    att_names = attr_info.name
  endif

  obj_destroy, shape
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

pro nrs_read_shape_polygons, shapes, polygons = pols, att_names = att_names, hint_geo = hint_geo
  compile_opt idl2, logical_predicate

  pols = []
  att_names = []

  if strlowcase(nrs_get_file_extension(shapes)) ne '.shp' then begin
    void = error_message('No shapefile specified')
    return
  endif

  ; open the input shapefile
  shape = obj_new('IDLffShape', shapes)

  ; Get the number of entities and the entity type.
  shape->idlffshape::getproperty, n_entities = num_ent, $
    entity_type = ent_type, n_attributes = num_attr

  if num_ent le 0 then begin
    obj_destroy, shape
    void = error_message('No features found in the shapefile')
    return
  endif

  pnt_types = [5, 15, 25]
  ix = where(ent_type eq pnt_types, pt_cnt)
  if pt_cnt eq 0 then begin
    obj_destroy, shape
    void = error_message('Only polygon features supported')
    return
  endif

  if num_attr gt 0 then begin
    shape->IDLffShape::GetProperty, attribute_info = attr_info
    att_names = attr_info.name
  endif
  pols = shape->IDLffShape::GetEntity(/all, /attributes)

  obj_destroy, shape
  
  minx = min((pols.bounds)[0, *])
  miny = min((pols.bounds)[1, *])
  maxx = max((pols.bounds)[4, *])
  maxy = max((pols.bounds)[5, *])
  hint_geo = ((abs(maxx) le 360.0 && abs(minx) le 360.0) $
    && (abs(maxy) le 360.0 && abs(miny) le 360.0))

end

function nrs_shape_attr_from, dt, width = width, dec = dec
  switch dt of
    1:
    2:
    12: begin
      width = 6
      dec = 0
      return, 3 ; integer
    end
    3:
    13: begin
      width = 11
      dec = 0
      return, 3 ; integer
    end
    14:
    15: begin
      width = 18
      dec = 0
      return, 3 ; integer
    end
    else: begin
      width = 15
      dec = 5
      return, 5 ; double
    end
  endswitch
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