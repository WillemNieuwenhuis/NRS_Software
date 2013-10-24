pro nrs_change_data_type, image, outname = outname, out_type = out_type, out_div = div $
                , prog_obj = prog_obj, cancelled = cancelled
  compile_opt idl2, logical_predicate

  cancelled = 0  
  nrs_set_progress_property, prog_obj, /start, title = 'Convert data type'
  
  envi_open_file, image, r_fid = fid
  envi_file_query, fid, dims = dims, nb = nb, nl = nl, ns = ns, interleave = interleave, data_type = dt
  inherit = envi_set_inheritance(fid, dims, /full)
  
  if n_elements(outname) eq 0 then $
    outname = getoutname(image, postfix = '_dt', ext = '.dat')

  openr, in_unit, image, /get_lun
  openw, out_unit, outname, /get_lun
  
  block_size = ns
  nr_elem = nb * nl
  
  in_block = make_array(block_size, type = dt, /nozero)
  out_block = make_array(block_size, type = out_type, /nozero)
  limits = nrs_minmax_from_datatype(out_type)

  for e = 0, nr_elem - 1 do begin
    if nrs_update_progress(prog_obj, e, nr_elem, cancelled = cancelled) then begin
      close, in_unit
      close, out_unit
      free_lun, in_unit
      free_lun, out_unit
      return
    endif

    readu, in_unit, in_block
    if out_type lt dt then begin
      in_block = (in_block < limits[1]) > limits[0]
    endif
    out_block[*] = in_block
    writeu, out_unit, out_block
  endfor
  
  envi_setup_head, fname = outname $
        , data_type = out_type $
        , ns = ns, nl = nl, nb = nb $
        , interleave = interleave $
        , /write $
        , inherit = inherit
  
  close, in_unit
  close, out_unit
  free_lun, in_unit
  free_lun, out_unit
end

pro run_now
  in = 'E:\NRS\Fangyuan\twi\yu\dem_l.dat'
  out = 'E:\NRS\Fangyuan\twi\yu\dem.dat'
  po = Obj_New("PROGRESSBAR", background = 'white', color = 'green' $
                        , ysize = 15, title = "Transform type" $
                        , /fast_loop $
                        )
  nrs_change_data_type, in, outname = out, out_type = 2, prog_obj = po, cancelled = cancelled
  
  po->Destroy
  
end