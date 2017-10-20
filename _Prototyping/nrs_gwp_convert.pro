pro nrs_GWP_convert, fn_in, fn_out
  compile_opt idl2
  
  ; Copy the map data, but not the color LUT
  ; So the image can load as gray instead of RGB
  img = read_tiff(fn_in, geotiff = geokeys)
  write_tiff, fn_out, img, compression = 1, geotiff = geokeys
  
  void = dialog_message('Done',  title = 'Convert Global Water Product RGB', /information)
  
end