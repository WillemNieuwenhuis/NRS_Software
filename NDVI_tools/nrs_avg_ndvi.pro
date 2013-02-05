pro nrs_aggr_stacks, img1, img2, img3
  envi_file_query, img1, nb = nb, nl = nl, ns = ns, dims = dims, fname = inp, data_type = dt
  mi = envi_get_map_info(fid = img1, undefined = undefined)
  outfile = getoutname(inp, postfix = '_avg', ext = '.')
  openw, unit, outfile, /get_lun
  for b = 0, nb - 1 do begin
    if dt eq 1 then $ ; in case of bytes use int type for calculation to avoid precision loss
      davg = fix(envi_get_data(fid = img1, dims = dims, pos = [b])) $
    else $
      davg = envi_get_data(fid = img1, dims = dims, pos = [b])
    davg += envi_get_data(fid = img2, dims = dims, pos = [b])
    davg += envi_get_data(fid = img3, dims = dims, pos = [b])
    if dt eq 1 then $
      writeu, unit, fix(davg / 3, type = dt) $  ; convert back to bytes
    else $
      writeu, unit, davg / 3
  endfor
  close, unit
  hdrfile = getOutname(outfile, ext = '.hdr', postfix = '') 
  envi_setup_head, data_type = dt $
      , fname = hdrfile $
      , map_info = mi $
      , nb = nb, ns = ns, nl = nl $
      , /write, interleave = 0
end