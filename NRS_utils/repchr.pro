;+
; :Description:
;   Replace all occurrences of one character in a string by a new character.
;   If no new character is specified, the space character is used as default
;
; :Returns:
;   Returns a string where all occurrences of the old character are
;   replaced by the new character
;
; :Params:
;   str : in
;     The string to replace the character in
;   old : in 
;     The character to replace
;   new : in
;     The new character replacing old one
;-
function repchr, str, old, new
  b = byte(str)
  old_b = byte(old)
  w = where(b eq old_b[0])
  if w[0] eq -1 then return, str
  
  if n_params(0) lt 3 then new = ' '
  new_b = byte(new)
  b[w] = new_b[0]
  
  return, string(b)
end
