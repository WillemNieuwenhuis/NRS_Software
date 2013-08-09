function nrs_minmax_from_datatype, dt
  compile_opt idl2, logical_predicate
  
  case dt of
    1  : return, [byte(0), byte(255)]
    2  : return, [fix(-32768), fix(32767)]
    3  : return, [long(-2147483648), long(2147483647)]
    14 : return, [long64(-9223372036854775808), long64(9223372036854775807)]
    4  : return, [float(-1.0e38), float(1.0e38)]
    5  : return, [double(-1.0d308), double(1.0d308)]
    12 : return, [uint(0), uint(65535)]
    13 : return, [ulong(0), ulong(4294967296)]
    15 : return, [ulong64(0), ulong64(18446744073709551615)]
    else :  return, []
  endcase
end
