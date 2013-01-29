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
