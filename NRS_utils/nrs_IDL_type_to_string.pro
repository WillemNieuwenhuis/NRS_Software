function nrs_IDL_type_to_string, typecode
  case typecode of
    0  : return, 'Undefined'
    1  : return, 'Byte'
    2  : return, 'Integer'
    3  : return, 'Long'
    4  : return, 'Float'
    5  : return, 'Double'
    6  : return, 'Complex (float)'
    7  : return, 'String'
    8  : return, 'Structure'
    9  : return, 'Complex (double)'
    10 : return, 'Pointer'
    11 : return, 'Object reference'
    12 : return, 'Unsigned Integer'
    13 : return, 'Unsigned Long'
    14 : return, '64-bit Integer'
    15 : return, 'Unsigned 64-bit Integer'
  endcase

  return, 'Invalid type' 
end

function nrs_IDL_sizeof, typecode
  compile_opt idl2, logical_predicate
  
  case typecode of
    0  : return, 0
    1  : return, 1
    2  : return, 2
    3  : return, 4
    4  : return, 4
    5  : return, 8
    6  : return, 8
    7  : return, 0  ; string
    8  : return, 0  ; structure
    9  : return, 16
    10 : return, 0  ; pointer
    11 : return, 0  ; object reference
    12 : return, 2
    13 : return, 4
    14 : return, 8
    15 : return, 8
  endcase
  
  return, 0
end

