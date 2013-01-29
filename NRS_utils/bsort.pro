;+
; NAME:
;       BSORT
; PURPOSE:
;       Function to sort data into ascending order, like a simple bubble sort.
; EXPLANATION:
;       Original subscript order is maintained when values are equal (FIFO).
;       (This differs from the IDL SORT routine alone, which may rearrange 
;       order for equal values)
;
;       A faster algorithm (radix sort) for numeric data is available  at 
;       http://idldatapoint.com/2012/04/19/an-lsd-radix-sort-algorithm-in-idl/
; CALLING SEQUENCE:  
;       result = bsort( array, [ asort, /INFO, /REVERSE ] )
;
; INPUT:
;       Array - array to be sorted
;
; OUTPUT:
;       result - sort subscripts are returned as function value
;
; OPTIONAL OUTPUT:
;       Asort - sorted array
;
; OPTIONAL KEYWORD INPUTS:
;       /REVERSE - if this keyword is set, and non-zero, then data is sorted
;                 in descending order instead of ascending order.
;       /INFO = optional keyword to cause brief message about # equal values.
;
; HISTORY
;       written by F. Varosi Oct.90:
;       uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
;       compatible with string arrays, test for degenerate array 
;       20-MAY-1991     JKF/ACC via T AKE- return indexes if the array to 
;                       be sorted has all equal values.
;       Aug - 91  Added  REVERSE keyword   W. Landsman      
;       Always return type LONG    W. Landsman     August 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
function bsort, array, asort, info=info, reverse = rev
  n = n_elements( array )
  if n lt 1 then begin
    print,'Input to bsort must be an array'
    return, [0l]
  endif
  
  if n lt 2 then begin
    asort = array       ;mdm added 24-sep-91
    return,[0l]         ;only 1 element
  end

  ; sort array (in descending order if reverse keyword specified )
  subs = sort(array)
  if keyword_set(rev) then subs = rotate(subs, 5)  
  asort = array[subs]

  ; now sort subscripts into ascending order
  ; when more than one asort has same value
  weq = where( (shift( asort, -1 ) eq asort) , neq ) 

  if keyword_set( info ) then $
    message, strtrim( neq, 2 ) + " equal values located", /con, /inf

  if (neq eq n) then return, lindgen(n) ;array is degenerate equal values
  
  if (neq gt 0) then begin
    if (neq gt 1) then begin              ;find clumps of equality
      wclump = where( (shift( weq, -1 ) - weq) gt 1, nclump )
      nclump = nclump + 1
    endif else nclump = 1

    if (nclump le 1) then begin
      clump_beg = 0
      clump_end = neq-1
    endif else begin
      clump_beg = [0,wclump+1]
      clump_end = [wclump,neq-1]
    endelse

    weq_beg = weq[ clump_beg ]              ;subscript ranges
    weq_end = weq[ clump_end ] + 1          ; of asort equalities.

    if keyword_set( info ) then message, strtrim( nclump, 2 ) + $
          " clumps of equal values located",/con,/inf

    for ic = 0l, nclump-1 do begin          ;sort each clump.
      subic = subs[ weq_beg[ic] : weq_end[ic] ]
      subs[ weq_beg[ic] ] = subic[ sort( subic ) ]
    endfor

    if n_params() ge 2 then asort = array[subs]     ;resort array.
  endif

  return, subs
end
