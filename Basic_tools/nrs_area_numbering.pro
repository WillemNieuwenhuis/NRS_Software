;+
; :Description:
;    Search for all connected pixels and give each conencted area a unique number.
;    If the input is a classified map with 'Unclassified', then the unclassified pixels
;    are used as a mask
;
; :Params:
;    image
;      FID of the image to number the areas on
;
; :Keywords:
;    areas : out
;      Result of the numbering proces
;    undef : out
;      No-data value for the numbering
;
; :Author: Willem Nieuwenhuis
;-
pro nrs_area_numbering, image, areas = areas, undef = iUNDEF, prog_obj = prog_obj
  compile_opt idl2, logical_predicate

  envi_file_query, image, dims = dims, ns = ns, nl = nl, fname = inputfile, class_names = cln
  mi = envi_get_map_info(fid = image)
  clx = where(cln eq 'Unclassified', cl_count)
  
  data = envi_get_data(fid = image, pos = [0], dims = dims)
  if cl_count gt 0 then $
    intern_mask = where(data eq clx[0], im_cnt)

  nrs_area_numbering_data, data, areas = areas, undef = iUNDEF
  
  if im_cnt gt 0 then $
    areas[intern_mask] = iUNDEF
end
  
;+
; :Description:
;    Search for all connected pixels and give each connected area a unique number.
;    Connections are made 4-connected (north, east, south, west)
;    No masking is done
;
; :Params:
;    data
;      The image data to number the areas on
;
; :Keywords:
;    areas : out
;      Result of the numbering proces
;    undef : out
;      No-data value for the numbering
;    prog_obj : in, optional
;      Progressbar object to indicate progress
;
; :Author: Willem Nieuwenhuis
;-
pro nrs_area_numbering_data, data, areas = areas, undef = iUNDEF, prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  doProgress = n_elements(prog_obj) gt 0
  cancelled = 0
  
  l = 0L                ; current line
  iLastColPlus1 = 0L    ; Col after final col of the current line track
  iArn = 0L             ; number of the current areanumbering
  iTempArn = 0L         ; temporary area number
  iCount = 0L           ; width of the current line track
  iUNDEF = -1L
  
  sz = size(data, /dim)
  ns = sz[0]
  nl = sz[1]
  temp_data = lonarr(ns, nl)
  
  iPrevArnLine = lonarr(ns + 2)
  iArnLine = lonarr(ns + 2) + iUNDEF ; set to undef == -1
  ArnToBeReplacedWith = [0]
  ArnRawAtt = [iUNDEF]

  iCurrLine = lonarr(ns + 2)
  iPrevLine = lonarr(ns + 2) + iUNDEF ; set to undef == -1
  for l = 0L, nl - 1 do begin
    if nrs_update_progress(prog_obj, l, nl, cancelled = cancelled) then return
    
    iCurrLine[1 : ns] = data[*, l]
    iCurrLine[0] = iUNDEF;
    iCurrLine[ns + 1] = iUNDEF;
    iValue = iCurrLine[1];
    iCount = 1;
    for iLastColPlus1 = 2, ns do begin
      if iValue eq iCurrLine[iLastColPlus1] then begin
        iCount++;
      endif else begin
        iTempArn = iAreaNumber(iValue, iArn, iLastColPlus1, iCount, iPrevLine, iPrevArnLine, ArnToBeReplacedWith, ArnRawAtt)

        for i = iLastColPlus1 - iCount, iLastColPlus1 do begin
          iArnLine[i] = iTempArn
        endfor
        
        iValue = iCurrLine[iLastColPlus1]
        iCount = 1
      endelse
    endfor
    
    iTempArn = iAreaNumber(iValue, iArn, iLastColPlus1, iCount, iPrevLine, iPrevArnLine, ArnToBeReplacedWith, ArnRawAtt)
    
    for i = iLastColPlus1 - iCount, iLastColPlus1 do begin
      iArnLine[i] = iTempArn
    endfor
    
    temp_data[*, l] = iArnLine[1 : ns]

    iPrevLine = iCurrLine
    iPrevArnLine = iArnLine
    iPrevArnLine[0] = iUNDEF;
    iPrevArnLine[ns + 1] = iUNDEF;
  endfor

  ; Merge phase: final areanumbering
  areas = lonarr(ns, nl)
  iFinalArn = 1L
  for iTempArn = 1L, iArn - 1 do begin
    if ArnToBeReplacedWith[iTempArn] eq 0 then begin
      ArnToBeReplacedWith[iTempArn] = iFinalArn++;   // add new areanumbering value
    endif else begin
      ArnToBeReplacedWith[iTempArn] = ArnToBeReplacedWith[ArnToBeReplacedWith[iTempArn]]; // take final areanumbering
    endelse
  endfor

  ; renumber
  for l = 0L, nl - 1 do begin
    iArnLine = temp_data[*, l]
    for i = 0L, ns - 1 do begin
      if iArnLine[i] ne iUNDEF then begin
        iArnLine[i] = ArnToBeReplacedWith[iArnLine[i]]
        iAreaNum = iArnLine[i]
      endif
    endfor
    areas[*, l] = iArnLine
  endfor
end

;+
; :Description:
;    Helper function to determine the next unique number for a connected area
;
; :Params:
;    iValue : in
;      The value of the current found area
;    iArn : out
;      The last unique number found (so far)
;    iLastColPlus1 : in
;      The position of the pixel under investigation
;    iCount : in, out
;      The size of the current connected area
;    iPrevLine : in
;      The raster line immediately above the current line
;    iPrevArnLine : in
;      The line with new numbers immediately above the current
;    ArnToBeReplacedWith : in, out
;      Array handling translation to areas that are candidates to be merged
;      with earlier found areas 
;    ArnRawAtt : in, out
;      Unused for now
;
;
; :Author: nieuwenhuis
;-
function iAreaNumber, iValue, iArn, iLastColPlus1, iCount, iPrevLine, iPrevArnLine, ArnToBeReplacedWith, ArnRawAtt
  compile_opt idl2, logical_predicate
  
  ; iTempArnChange = temporary store of the areanumbering to change
  ; iTempArnNew = temporary store of the new areanumbering
  ;
  iUNDEF = -1
  if iValue eq iUNDEF then return, iUNDEF
  
  iSta = iLastColPlus1 - iCount ; start of current segment
  iEnd = iLastColPlus1 - 1      ; end of current segment
  iTempArn = 0                  ; temporary arenumbering of the current segment
  j = iSta                      ; current col of the segment
  for j = iSta, iEnd do begin
    if iValue eq iPrevLine[j] then begin
      if iTempArn eq 0 then begin   ; take area number
        iTempArn = iPrevArnLine[j]
      endif else begin
        if iTempArn ne iPrevArnLine[j] then begin
          ; have to list change combination
          if iTempArn gt iPrevArnLine[j] then begin
            iTempArnChange = iTempArn
            iTempArnNew = iPrevArnLine[j]
            iTempArn = iPrevArnLine[j]
          endif else begin
            iTempArnChange = iPrevArnLine[j]
            iTempArnNew = iTempArn
          endelse
          while 1 do begin
            if ArnToBeReplacedWith[iTempArnChange] eq 0 then begin
              ArnToBeReplacedWith[iTempArnChange] = iTempArnNew
              break;                // new change
            endif
            if ArnToBeReplacedWith[iTempArnChange] eq iTempArnNew then $
              break;   // change combination already exists

            ; creates new change combination with the new areanumbering values.
            ;   Case 1: if 8->3 comb. exists already, and 8->5 is new change comb,
            ;           then new change comb. 5->3 is added, and change comb. 8->3 remains.
            ;   Case 2: if 9->5 comb. exists already, and 9->2 is new change comb,
            ;           then new change comb. 5->2 is added, and change comb. 9->5 is
            ;           replaced by 9->2.
            ;           The search continues until it could store a change comb. or an already
            ;           existing is found.

            if ArnToBeReplacedWith[iTempArnChange] lt iTempArnNew then begin
              h  = iTempArnChange
              iTempArnChange = iTempArnNew
              iTempArnNew = ArnToBeReplacedWith[h]
            endif else begin
              h = iTempArnChange
              iTempArnChange = ArnToBeReplacedWith[iTempArnChange]
              ArnToBeReplacedWith[h] = iTempArnNew
            endelse
          endwhile
        endif
      endelse
    endif
  endfor

  if iTempArn eq 0 then begin ; no connection found: create new number
    iTempArn = ++iArn
    ArnToBeReplacedWith = [ArnToBeReplacedWith, 0]
    ArnRawAtt = [ArnRawAtt, iValue]
  endif
  return, iTempArn
end
