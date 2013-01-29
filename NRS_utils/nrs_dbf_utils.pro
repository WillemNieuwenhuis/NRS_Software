;+
; :Description:
; read a DBF table, and extract point locations. The
; columns that contain the 2D coordinates are named in the
; X/Y_colnames
; 
; :Params:
;   points: [out]
;    the point locations found in the dbf
;   dbf_name: [in]
;     the name of the dbf file
;   X_colname: [in]
;    the name of the column containing the X-coordinate
;   Y_colname: [in]
;    the name of the column containing the Y-coordinate
;-
pro nrs_read_dbffile, points, dbf_name, X_colname, Y_colname
  point = {SinglePoint, $
    x:  double(0),  $
    y:  double(0),  $
    px: long(0),  $
    py: long(0)   $
  }
  ; open the input DBF
  myshape = OBJ_NEW('IDLffShape', dbf_name, /DBF_ONLY)

    ; Get the number of entities and the entity type.
  myshape->IDLffShape::GetProperty, n_records = num_ent, attribute_info = att_info

  x_index = -1
  y_index = -1
  for field = 0, n_elements(att_info) - 1 do begin
    if att_info[field].name eq X_colname then x_index = field
    if att_info[field].name eq Y_colname then y_index = field
  end

  rstr = ["Reading point locations from dBase file"]
  envi_report_init, rstr, base = tranq, title = "Copying"

  points = replicate(point, num_ent)
  for i = 0, num_ent - 1 do begin
    envi_report_stat, tranq, i, num_ent
    ; get the feature and its attributes
    attr = myshape->IDLffShape::GetAttributes(i)

    ; get the coordinate (assumption it is a point)
    points[i].x = attr[0].(x_index)
    points[i].y = attr[0].(y_index)
  endfor
  envi_report_stat, tranq, num_ent, num_ent

  ; Close the source Shapefile.
  OBJ_DESTROY, myshape

  envi_report_init, base = tranq, /finish
end
