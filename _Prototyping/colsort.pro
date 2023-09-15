; docformat = 'rst'
;+
; :description:
;	Applies SORT (ascending order) to a selected column of a two-dimensional 
;	array, then applies this sort to all the columns of the array. (It's like
;	doing a column sort in Excel.)
;
; :params:
;    array: in, required, type=numeric
;        A two-dimensional numeric array. 
;    column_index: in, required, type=integer
;        The column index over which sorting is performed.
;
; :keywords:
;     reverse_sort: in, optional, type=boolean
;        Set this keyword to do a reverse sort (descending order) on the
;        elements of the selected column. 
;
; :author:
;	Mark Piper, ITT VIS, 2009
;
; :version:
;  $Id: colsort.pro 182 2009-11-03 21:44:28Z mpiper $
;-
function colsort, array, column_index, reverse_sort=rsort
   compile_opt idl2
   on_error, 2

   info = size(array, /structure)

   ; Test dimensions of input array.
   if info.n_dimensions ne 2 then $
      message, 'Input array must have two dimensions.'
      
   ; Ensure the sorting index is within a valid range.
   if n_elements(column_index) eq 0 then $
      message, 'Must pass a column index for sorting.'
   n_columns = info.dimensions[0]  
   _column_index = (column_index > 0) < (n_columns-1)    
   
   ; Sort the desired column.
   col_sort_index = sort(array[_column_index,*])
   if keyword_set(rsort) then $
      col_sort_index = reverse(col_sort_index)

   ; Apply the sort to each column of the array.
   sorted_array = array
   for i=0, n_columns-1 do $
      sorted_array[i,*] = (array[i,*])[col_sort_index]
   
   return, sorted_array
end


;+
; Example main. Examine the results in the Console.
;-
a = round(randomu(123, 5, 6) * 20.0)
sort_index = 1
b = colsort(a, sort_index, /reverse_sort)
print, 'Original array:'
print, a
print
print, 'Column index to sort on: ', sort_index
print
print, 'Sorted (reverse) array:'
print, b
end
