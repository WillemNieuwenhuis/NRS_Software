;+
; :description:
;    Calculate the boundaries of a selection of cells around a centre cell location
;    with a user defined kernel size. The center is located at (0, 0).
;    <p>
;    There are two options for the user defined kernel shape:
;    - square with equal sides (size = kernel size)
;    - circle with diameter of kernel size.
;    <p>
;    The calculated selection is returned in the select keyword. This is a table with three columns
;    and kernel size number of rows. The first row indicates the minimum vertical offset from the
;    center of the kernel.
;    - column 1: contains the number of selected cells
;    - column 2: the minimum horizontal offset from the center
;    - column 3: the maximum horizontal offset from the center
;    <p>
;    Note that only filled shapes are supported.
;
; :params:
;    kernel : in, required
;     The size of the kernel in cells
;    kern_type : in, required
;      The shape of the kernel, either square (0) or circle (1)
;
; :keywords:
;    select : out
;      Place holder for the calculated selection
;
; :author: nieuwenhuis
; :history:
;   - Aug 2015 - created
;-
pro nrs_aggregate_spectra_select, kernel, kern_type, select = select
  compile_opt idl2, logical_predicate

  kern2 = fix(kernel / 2)
  select = lonarr(3, kernel)
  if kern_type eq 0 then begin
    ; square
    select[0, *] = kernel
    select[1, *] = -kern2
    select[2, *] = kern2
  endif
  if kern_type eq 1 then begin
    ; circle
    kk = indgen(kernel) - kern2
    select[1, *] = - floor(sqrt(kern2 ^ 2 - kk ^ 2))
    select[2, *] = floor(sqrt(kern2 ^ 2 - kk ^ 2))
    select[0, *] = select[2, *] - select[1, *] + 1
  endif
end

