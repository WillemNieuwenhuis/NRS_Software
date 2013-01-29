;+
; :Description:
;   Select a shapefile and open it, returning details through the keyword parameters
; 
; :Params:
;   myshape
;     Reference to the open shapefile object
;     
; :Keywords:
;   filename:  in, optional
;     filename of the shapefile; if not specified, or the file does not exist
;     a file open dialog is displayed.
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
pro openShapefile, myshape, num_ent, ent_type, num_attr, attr_info, filename = filename
  notFound = 1
  if n_elements(filename) gt 0 then begin
    notFound = 1 - (file_info(filename)).exists
  endif
  if notFound eq 1 then begin
    ;Open the Shapefile
    filter = [['*.shp'], ['ESRI shapefiles']]
    filename = dialog_pickfile(title = "Open shape file", /read, filter = filter, $
          default_extension = "shp")  ; make sure the shapefile has an extension
    if strlen(filename) eq 0 then retall ; no name specified, leave application
  endif

  ; open the input shapefile
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
;-
pro createNewShapefile, inputShape, newShape
  inputShape->IDLffShape::GetProperty, FILENAME = inputName
  
  ; Get the number of entities and the entity type.
  inputShape->IDLffShape::GetProperty, N_ENTITIES = num_ent, $
      ENTITY_TYPE = ent_type, N_ATTRIBUTES = num_attr

  ; Get the info for all attributes (if any).
  if num_attr gt 0 then begin
    inputShape->IDLffShape::GetProperty, ATTRIBUTE_INFO = attr_info
  end

  outputShapeName = getOutputShapeName(inputName)

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

