; Split a single band image into smaller rectangular images
; The input image can have any rectangular size
; The output images will be square except for possibly the images split
; of at the end of the sides.
; The output filename is <inputname>_yyy_xxx
; where yyy is the number of the image in the Y direction
; and xxx is the number of the image in the x direction
; 
; If a coordinate system is attached the proper offset is calculated
pro nrs_split_image, fid, new_size, list = list
  envi_file_query, fid, ns = ns, nl = nl, fname = filename
  mapinfo = envi_get_map_info(fid = fid, undefined = undefined)
  
  x_size = ceil(1.0 * ns / new_size)
  y_size = ceil(1.0 * nl / new_size)
  nr_files = x_size * y_size 
  
  dims = lonarr(5)
  for y = 0, y_size - 1 do begin
    for x = 0, x_size - 1 do begin
      outname = filename + '_' + string(y, format = '(i03)') + '_' + string(x, format = '(i03)')
      if x + y eq 0 then list = [outname] $
      else list = [list, outname]
      dims[0] = -1
      dims[1] = x * new_size
      dims[2] = nrs_clip_to_bounds(x * new_size + new_size - 1, ns)
      dims[3] = y * new_size
      dims[4] = nrs_clip_to_bounds(y * new_size + new_size - 1, nl)
      data = envi_get_data(fid = fid, pos = [0], dims = dims)
      if undefined eq 0 then begin
        mi = mapinfo
        mi.mc[2] += mi.ps[0] * new_size * x
        mi.mc[3] -= mi.ps[1] * new_size * y
        envi_write_envi_file, data, out_name = outname, map_info = mi, /no_open, /no_copy
      endif else begin
        envi_write_envi_file, data, out_name = outname, /no_open, /no_copy
      endelse
    endfor
  endfor
end