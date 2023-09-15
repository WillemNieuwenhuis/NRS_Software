function parse_readme, folder
  compile_opt idl2, logical_predicate

  file_mask = folder + path_sep() + '*readme.xml'
  xml_files = file_search(count = file_count, file_mask)
  if file_count eq 0 then begin
    return, []
  endif
  
  ; Only one readme expected, but select the first anyway
  xml = xml_files[0]
  p = obj_new('IDLffXMLDOMDocument')
  p->load, filename = xml, schema_checking = 0, /quiet

  oTopLevel = p->getDocumentElement() ;return root IDLffXMLDOMDocument

  files = []
  node = otoplevel->getElementsByTagName('FILE')
  nr_files = node->getlength()
  for i = 0, nr_files - 1 do begin
    item = (node->item(i))->getFirstChild()
    filename = item->getNodeValue()
    if filename.Matches('_R[0-9]{1,2}C[0-9]{1,2}-') then begin
      files = [files, filename]
    endif
  endfor
  files = files.extract('.*(MUL|PAN).*_(R[0-9]{1,2}C[0-9]{1,2})-.*', /sub)
  ; sort data by rowcol
  files = colsort(files, 2)
  rowcol_all = files[2,*]
  
  rc_uniq = rowcol_all[uniq(rowcol_all)]
  gs_files = make_array(2, n_elements(rc_uniq), /string)
  for rc = 0, n_elements(rc_uniq) - 1 do begin
    rowcol = rc_uniq[rc]
    ix = where(files[2, *] eq rowcol)
    f1 = files[*, ix[0]]
    f2 = files[*, ix[1]]
    pan_file = f1[0]
    mul_file = f2[0]
    if f1[1] ne 'PAN' then begin
      pan_file = f2[0]
      mul_file = f1[0]
    endif
    gs_files[*, rc] = [pan_file, mul_file]
  endfor
  return, gs_files
end

;+
; :Description:
;    Run the Gramm-Schmidt pan-sharpening on
;    WorldView2 multispectral and panchromatric data  
;
; :Params:
;    root_folder: indicate the folder containing the data folders for the PAN data and
;       the multispectral data and where the readme.xml is located.
;       fe: <local folder>\1_104001008BC29100\015975768010_01_003\015975768010_01_003\015975768010_01
;
; :Keywords:
;   resample_method: 'Nearest Neighbor', 'Bilinear', 'Cubic Convolution' 
;
; :Author: nieuwenhuis
; 
; :History:
;   - created september 2023
;-
pro GS_pan_worldview2_batch, root_folder, resample_method = resample, overwrite = overwrite
  compile_opt idl2, logical_predicate

  e = envi(/headless)

  out_folder = root_folder + path_sep() + 'pan_sharp'
  if ~file_test(out_folder, /directory) then $
    file_mkdir, out_folder
  raw_files = file_search(count = file_count, out_folder + path_sep() + '*.*')
  if file_count gt 0 then begin
    if keyword_set(overwrite) then begin
      file_delete, raw_files
    endif
  endif

  file_list = parse_readme(root_folder)

  Task = ENVITask('GramSchmidtPanSharpening')
  Task.sensor = 'worldview2'
  if arg_present(resample_method) then $
    Task.resampling = resample    ; default = bilinear

  nr_files = n_elements(file_list[0,*])
  for f = 0, nr_files - 1 do begin
    mul_file = root_folder + path_sep() + file_list[1, f]
    outname = out_folder + path_sep() + getoutname(file_basename(mul_file), ext = '.tif', postfix = '_gs')
    ; skip processing if output is already created.
    if file_test(outname, /regular) then begin
      print, 'Skipping: ', mul_file
      continue
    endif
 
    mul_raster = e.OpenRaster(mul_file)
    mul_rgbi = ENVISubsetRaster(mul_raster, BANDS=[1, 2, 4, 6]) ; B,G,R,NIR 

    pan_file = root_folder + path_sep() + file_list[0, f]
    pan_raster = e.OpenRaster(pan_file)

    Task.input_low_resolution_raster = mul_rgbi
    Task.input_high_resolution_raster = pan_raster
    Task.output_raster_uri = ''  ; leave empty, we are creating tiff file instead

    Task.Execute
    
    raster = Task.output_raster
    raster.export, outname, 'TIFF'

    ; remove temporary large data files
    e.CleanupTemporaryWorkspace
  endfor
  
end

pro gs_batch
;  GS_pan_worldview2_batch, 'F:\Data\Tiejun\1_104001008BC29100\015975768010_01_003\015975768010_01_003\015975768010_01'
;  GS_pan_worldview2_batch, 'F:\Data\Tiejun\2_104001008AB0C300\015975739010_01_003\015975739010_01_003\015975739010_01'
  GS_pan_worldview2_batch, 'F:\Data\Tiejun\3_1040010089383500\015975757010_01_003\015975757010_01_003\015975757010_01'
  GS_pan_worldview2_batch, 'F:\Data\Tiejun\4_1040010089C37300\015975756010_01_003\015975756010_01_003\015975756010_01'
end
