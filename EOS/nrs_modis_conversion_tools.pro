;+
; :Description:
;    Convert MODIS MCD43A4 product to ENVI format.
;    The name of the input without extension is used as rootname for the converted product.
;    The output name will be: <input>_Grid_2D.dat
;
; :Params:
;    modis_img : in, required
;      The name of the MODIS hdf file
;    out_folder : out, required
;      The name of the output folder to store the converted product
;
; :Keywords:
;    close_file : in, optional, default = yes
;      If set to yes (non-zero) then close the converted product. If
;      set to no (zero) then leave the converted product open. Useful for bor batch processing.
;
; :Author: nieuwenhuis
; 
; :History:
;   - 16 aug 2017: created
;-
pro nrs_modis_convert_grid, modis_img, out_folder, close_file = close_file
  compile_opt idl2, logical_predicate

  if n_elements(close_file) eq 0 then close_file = 1
  
  nr_grids = eos_gd_inqgrid(modis_img, grids)
  if nr_grids le 0 then return
  
  fid = eos_gd_open(modis_img, /read)
  if fid eq -1 then return
  
  ; only process the first grid found
  gid = eos_gd_attach(fid, grids[0])
  nr_fields = eos_gd_inqfields(gid, fields, nr_dims, num_types)
  isok = eos_gd_detach(gid)
  isok = eos_gd_close(fid)
    
  ; MCD43A4 specific names
  grid_name = grids[0]
  sd_names = strsplit(fields, ',', /extract)
  ; end MCD43A4 specific names

  ;Output method schema is:
  ;0 = Standard, 1 = Reprojected, 2 = Standard and reprojected
  out_method = 0

  output_rootname = getoutname(modis_img, ext = ' ', postfix = '')
  output_rootname = file_basename(output_rootname)
  ;Set reprojection background and any native fill values to NaN
  nan_fill = float('NaN')
  convert_modis_data, in_file=modis_img, $
    out_path = out_folder, $
    out_root = output_rootname, $
    gd_name = grid_name, $
    sd_names = sd_names, $
    out_method = out_method, $
    background = nan_fill, $
    fill_replace_value = nan_fill, $
    r_fid_array = fids

  if keyword_set(close_file) then begin
    for f = 0, n_elements(fids) - 1 do envi_file_mng, id = fids[f], /remove
  endif
end

pro nrs_modis_batch_convert_grid, base_folder, out_folder, progress = progress
  compile_opt idl2

  files = nrs_find_images(base_folder, 'MCD,MOD,MYD', ext = 'hdf')
  if n_elements(files) eq 0 then begin
    void = error_message('No MODIS products in ' + base_folder, /error)
    return
  endif

  if keyword_set(progress) then begin
    ; initialise tranquilizer
    prog_obj = obj_new("PROGRESSBAR", background = 'white', color = 'green' $
      , ysize = 15, title = 'Batch MODIS conversion' $
      , /fast_loop $
      )
  endif

  nrs_set_progress_property, prog_obj, /start, title = 'Batch MODIS conversion'

  for f = 0, n_elements(files) - 1 do begin
    if nrs_update_progress(prog_obj, f, n_elements(files), cancelled = cancelled) then begin
      break
    endif
    nrs_modis_convert_grid, files[f], out_folder
  endfor
  
  if obj_valid(prog_obj) then prog_obj->destroy

end
