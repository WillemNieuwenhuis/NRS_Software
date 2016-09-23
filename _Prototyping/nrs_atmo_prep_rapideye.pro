;+
; :description:
;    Preprocess RapidEye image to turn DN into TOA reflectance. This is
;    specifically designed for the 3A products without atmospheric correction.
;    
;    The reflectance values are cropped to 100% to eliminate the higher values that 
;    are possible in case of cloud coverage.
;
; :params:
;    image : the RapidEye image
;    sundist : the earth sun distance in astronomical units at the date of acquisition
;    solar_illumination : the solar illumination angle in degrees
;
;
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
;-
pro nrs_atmo_prep_rapideye, image, sundist, solar_illumination
  compile_opt idl2, logical_predicate

  envi_open_file, image, r_fid = fid
  if fid eq -1 then begin
    void = error_message('Could not find image')
    return
  endif

  envi_file_query, fid, nb = nb, ns = ns, nl = nl, dims = dims  
  meta = envi_set_inheritance(fid, dims, /full)
  if nb ne 5 then begin
    void = error_message('Number of bands should be 5 for RapidEye')
    return
  endif
  ; input filename looks like: 3263807_2015-03-01_RE3_3A_298788.tif
  ; where:
  ;   3263807 is the tile number
  ;   2015-03-01 is the acquisition date
  ; Output filename will be created as: toa_3263807_2015_060.dat
  ; where:
  ;   toa indicates Top Of Atmosphere
  ;   3263807 is the tile number
  ;   2015 is the year of acquisition
  ;   060 is the day of year of acquisition
  parts = strsplit(file_basename(image), '_', /extract)
  date_parts = fix(strsplit(parts[1], '-', /extract))
  doy = julday(date_parts[1], date_parts[2], date_parts[0]) - julday(1, 1, date_parts[0]) + 1
  outpath = file_dirname(image, /mark)
  outbasename =  parts[0] $
                 + '_' + string(date_parts[0], format = '(i0)') $
                 + '_' + string(doy, format = '(i03)') + '.dat'
  
  EAI = [1197.8, 1863.5, 1560.4, 1395.0, 1124.4]  ; [blue, green, red, red_edge, NIR]  
  bnames = ['blue_', 'green_', 'red_', 'red_edge_', 'NIR_']
  solar_zenith = (90.0 - solar_illumination) * !PI / 180.0 ; angle conversion and degree to radians
  ; preprocessing:
  ;  - DN to radiance
  ;  - radiance to TOA reflectance
  for b = 0, nb - 1 do begin
    data = envi_get_data(fid = fid, dims = dims, pos = [b])
    ; DN to radiance
    data *= 0.01
    
    ; radiance to TOA reflectance
    ref = data * !PI * sundist ^ 2 / (EAI[b] * cos(solar_zenith))
    ; mask reflectance higher than 100%
    ref = ref < 1.0
    
    outname = outpath + 'toaf_' + bnames[b] + outbasename
    envi_write_envi_file, ref, out_name = outname, inherit = meta, /no_open, /no_copy
  endfor

end

;+
; :description:
;   Batch run of Raw RapidEye 3A to top of atmosphere preprocessing.
;   All data on illumination elevation angle and sun distance are from:
;   'parse RapidEye metadata.xlsx'
;   
;   The run is separated into sections for UK, NL and both DE sites
;
; :history:
;   - January 2016: created, WN
;-
pro nrs_atmo_prep_batch
  compile_opt idl2, logical_predicate

  ; UK site (tile: 3062512)
  print, 'UK site'
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-03-07_RE2_3A_298851.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-04-15_RE3_3A_300283.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-04-21_RE5_3A_301243.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-05-16_RE1_3A_303764.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-05-27_RE2_3A_303764.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-06-19_RE2_3A_304390.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-06-30_RE3_3A_304722.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-07-10_RE4_3A_305135.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-08-08_RE4_3A_306306.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-09-06_RE5_3A_309636.tif' $
  ]
  sun_dist_list = [0.9923392, 1.0032519, 1.0049165, 1.0110806, 1.0132118 $
                 , 1.0161048, 1.0166476, 1.0166408, 1.0139912, 1.0080019]
  illum_elev_list = [34.08492, 49.14297, 51.1562, 58.47802, 60.63029 $
                   , 62.70034, 62.49932, 61.30763, 55.42677, 45.75595]
  for f = 0, n_elements(files) -1 do begin
    print, file_basename(files[f])
    nrs_atmo_prep_rapideye, files[f], sun_dist_list[f], illum_elev_list[f]
  endfor
  
  ; NL site (tile: 3263807)
  print, 'NL site'
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-03-01_RE3_3A_298788.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-03-10_RE2_3A_299086.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-04-05_RE5_3A_300279.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-04-18_RE3_3A_302344.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-05-10_RE1_3A_302207.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-06-12_RE1_3A_303682.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-06-29_RE4_3A_304388.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-07-02_RE2_3A_305138.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-07-18_RE4_3A_305965.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-08-09_RE2_3A_307288.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-09-09_RE5_3A_308427.tif' $
  ]
  sun_dist_list = [0.99084, 0.9931204, 1.0004053, 1.00409, 1.0097481 $
                   , 1.0154668, 1.0166219, 1.0166844, 1.0162939, 1.0138344, 1.0072409]
  illum_elev_list = [28.8894, 32.47988, 42.59454, 47.409, 54.1336 $
                   , 59.74837, 59.77831, 59.64056, 57.51854, 52.45207, 41.91416]
  for f = 0, n_elements(files) -1 do begin
    print, file_basename(files[f])
    nrs_atmo_prep_rapideye, files[f], sun_dist_list[f], illum_elev_list[f]
  endfor

  ; DE site (tile: 3361610)
  print, 'DE site 1'
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-03-19_RE4_3A_299694.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-04-09_RE1_3A_300932.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-04-10_RE2_3A_300932.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-05-08_RE1_3A_303063.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-05-13_RE1_3A_303063.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-06-14_RE5_3A_305334.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-07-02_RE4_3A_305137.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-07-17_RE5_3A_305963.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-06_RE1_3A_306189.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-08_RE3_3A_307279.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-26_RE2_3A_308911.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-09-13_RE1_3A_308426.tif' $
  ]
  sun_dist_list = [0.9955642, 1.0015508, 1.0018362, 1.0092806, 1.0104283, 1.0156716 $
                 , 1.0166844, 1.0163537, 1.0142927, 1.0139912, 1.0105957, 1.0061965]
  illum_elev_list = [40.47147, 48.79194, 49.18777, 58.40453, 59.67386, 64.55141 $
                   , 64.17859, 62.20642, 57.82113, 57.23247, 51.67897, 45.12145]
  for f = 0, n_elements(files) -1 do begin
    print, file_basename(files[f])
    nrs_atmo_prep_rapideye, files[f], sun_dist_list[f], illum_elev_list[f]
  endfor

  ; DE site (tile: 3361710)
  print, 'DE site 2'
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-03-19_RE4_3A_299694.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-04-10_RE2_3A_300932.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-05-13_RE1_3A_303063.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-06-14_RE5_3A_305334.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-07-02_RE4_3A_305137.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-07-17_RE5_3A_305963.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-08-06_RE1_3A_306189.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-08-26_RE2_3A_308911.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-09-13_RE1_3A_308426.tif' $
  ]
  sun_dist_list = [0.9955642, 1.0018362, 1.0104283, 1.0156716, 1.0166844 $
                 , 1.0163537, 1.0142927, 1.0105957, 1.0061965]
  illum_elev_list = [40.25565, 48.97132, 59.45889, 64.33499, 63.96298 $
                   , 61.99173, 57.60552, 51.46263, 44.90502]
  for f = 0, n_elements(files) -1 do begin
    print, file_basename(files[f])
    nrs_atmo_prep_rapideye, files[f], sun_dist_list[f], illum_elev_list[f]
  endfor

  
end

;+
; :description:
;    Stack the atmospheric corrected images in the original order 
;    For the UK site
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
pro nrs_atmo_post_stack_UK
  compile_opt idl2, logical_predicate

  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_blue_3062512_2015_249.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_green_3062512_2015_249.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_3062512_2015_249.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_red_edge_3062512_2015_249.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_NIR_3062512_2015_249.dat' ]
  stacks = [ $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_249.dat' ]

  nr_stack = n_elements(stacks)
  files = reform(files, 5, nr_stack, /over)
  for st = 0, nr_stack -1 do begin
    nrs_stack_image, stacks[st], list_file = files[*, st], band_names = ['blue', 'green', 'red', 'red_edge', 'NIR'] 
  endfor
end


;+
; :description:
;    Stack the atmospheric corrected images in the original order
;    For the NL site 
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
;-
pro nrs_atmo_post_stack_NL
  compile_opt idl2, logical_predicate

  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_blue_3263807_2015_252.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_green_3263807_2015_252.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_3263807_2015_252.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_red_edge_3263807_2015_252.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_NIR_3263807_2015_252.dat' $
    ]
  stacks = [ $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_252.dat' $
    ]

  nr_stack = n_elements(stacks)
  files = reform(files, 5, nr_stack, /over)
  for st = 0, nr_stack -1 do begin
    nrs_stack_image, stacks[st], list_file = files[*, st], band_names = ['blue', 'green', 'red', 'red_edge', 'NIR']
  endfor
end

;+
; :description:
;    Stack the atmospheric corrected images in the original order 
;    For both the DE sites 
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
pro nrs_atmo_post_stack_DE
  compile_opt idl2, logical_predicate

  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361610_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361610_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361610_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361610_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361610_2015_256.dat' $
    ]
  stacks = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_256.dat' $
   ]
  nr_stack = n_elements(stacks)
  files = reform(files, 5, nr_stack, /over)
  for st = 0, nr_stack -1 do begin
    nrs_stack_image, stacks[st], list_file = files[*, st], band_names = ['blue', 'green', 'red', 'red_edge', 'NIR']
  endfor
  
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_blue_3361710_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_green_3361710_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_3361710_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_red_edge_3361710_2015_256.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_NIR_3361710_2015_256.dat' $
  ]
  stacks = [ $
  'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_256.dat' $
  ]
  
  nr_stack = n_elements(stacks)
  files = reform(files, 5, nr_stack, /over)
  for st = 0, nr_stack -1 do begin
    nrs_stack_image, stacks[st], list_file = files[*, st], band_names = ['blue', 'green', 'red', 'red_edge', 'NIR']
  endfor
  
end

;+
; :description:
;    Resize the mask files to the same resolution as the actual RapidEye images
;    mask = 50 x 50 , original = 5 x 5 (all meters)
;    Then apply the mask to the atmospherically corrected images
;    Note: the reflectances will be cropped to zero (for negative values)
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
;-
pro nrs_atmo_post_mask_resize_batch
  compile_opt idl2, logical_predicate

  ; UK site; note that the files and mask list should be in the same time order!
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_066.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_105.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_111.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_136.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_147.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_170.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_181.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_191.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\UK\toc_3062512_2015_249.dat' $
  ]
  masks = [ $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-03-07_RE2_3A_298851_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-04-15_RE3_3A_300283_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-04-21_RE5_3A_301243_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-05-16_RE1_3A_303764_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-05-27_RE2_3A_303764_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-06-19_RE2_3A_304390_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-06-30_RE3_3A_304722_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-07-10_RE4_3A_305135_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-08-08_RE4_3A_306306_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\UK\3062512_2015-09-06_RE5_3A_309636_udm.tif' $
  ]
  nrs_atmo_post_mask_resize, files, masks

  ; NL site; note that the files and mask list should be in the same time order!
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_060.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_069.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_095.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_108.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_130.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_163.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_180.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_199.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_221.dat', $
    'E:\NRS_processing\RapidEye\Allsites\NL\toc_3263807_2015_252.dat' $
  ]    
  masks = [ $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-03-01_RE3_3A_298788_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-03-10_RE2_3A_299086_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-04-05_RE5_3A_300279_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-04-18_RE3_3A_302344_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-05-10_RE1_3A_302207_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-06-12_RE1_3A_303682_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-06-29_RE4_3A_304388_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-07-02_RE2_3A_305138_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-07-18_RE4_3A_305965_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-08-09_RE2_3A_307288_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\NL\3263807_2015-09-09_RE5_3A_308427_udm.tif' $
  ]
  nrs_atmo_post_mask_resize, files, masks

  ; DE site, tile 1; note that the files and mask list should be in the same time order!
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_099.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_128.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_220.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361610_2015_256.dat' $
  ]
  masks = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-03-19_RE4_3A_299694_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-04-09_RE1_3A_300932_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-04-10_RE2_3A_300932_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-05-08_RE1_3A_303063_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-05-13_RE1_3A_303063_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-06-14_RE5_3A_305334_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-07-02_RE4_3A_305137_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-07-17_RE5_3A_305963_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-06_RE1_3A_306189_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-08_RE3_3A_307279_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-08-26_RE2_3A_308911_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361610_2015-09-13_RE1_3A_308426_udm.tif' $
  ]
  nrs_atmo_post_mask_resize, files, masks

  ; DE site, tile 2; note that the files and mask list should be in the same time order!
  files = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_078.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_100.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_133.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_165.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_183.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_198.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_218.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_238.dat', $
    'E:\NRS_processing\RapidEye\Allsites\DE\toc_3361710_2015_256.dat' $
  ]
  masks = [ $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-03-19_RE4_3A_299694_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-04-10_RE2_3A_300932_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-05-13_RE1_3A_303063_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-06-14_RE5_3A_305334_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-07-02_RE4_3A_305137_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-07-17_RE5_3A_305963_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-08-06_RE1_3A_306189_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-08-26_RE2_3A_308911_udm.tif', $
    'E:\NRS_processing\RapidEye\Allsites\DE\3361710_2015-09-13_RE1_3A_308426_udm.tif' $
  ]
  nrs_atmo_post_mask_resize, files, masks
end

;+
; :description:
;    Resize a mask image to the same resolution as the actual RapidEye images
;    mask = 50 x 50 , original = 5 x 5 (all meters)
;    Then apply the mask to a atmospherically corrected image
;    Note: negative reflectances will be cropped to zero
;
; :params:
;    files  : in, required
;    masks
;
; :author: nieuwenhuis
; :history:
;   - January 2016: created, WN
pro nrs_atmo_post_mask_resize, files, masks
  compile_opt idl2, logical_predicate

  for f = 0, n_elements(masks) - 1 do begin
    f_name = files[f]
    m_name = masks[f] ; m_name and f_name are linked!
    out_path = file_dirname(f_name, /mark)
    out_base = file_basename(f_name)  ; toc_<tile>_<year>_<doy>.dat
    ; determine the output mask filename and final output (masked top of canopy)
    out_mask = out_path + 'mask_' + strmid(out_base, 4)
    out_name = out_path + 'mtoc_' + strmid(out_base, 4)
    
    envi_open_file, f_name, r_fid = fid_toc
    if fid_toc eq -1 then continue
    
    envi_open_file, m_name, r_fid = fid_inmask
    if fid_inmask eq -1 then continue

    envi_file_query, fid_inmask, dims = dims_small
    ; first make the mask the same size as the image and also store it on disk
    envi_doit, 'resize_doit', dims = dims_small, fid = fid_inmask, r_fid = fid_outmask $
                            ,  out_name = out_mask, rfact=[0.1, 0.1], pos = [0];, interp= 0

    envi_file_query, fid_toc, nb = nb, nl = nl, ns = ns, bnames = bnames, dims = dims
    ; build mask per band; always use bit 0 (black fill), bit 1 (cloud)
    ; bit 2 for blue, bit 3 for green, bit 4 for red, bit 5 for rededge, bit 6 for NIR
    mdata = bytarr(ns, nl, nb)
    data = envi_get_data(fid = fid_outmask, dims = dims, pos = [0])
    mdata[*, *, 0] = 1 - ((data and 7) gt 0)   ; blue      
    mdata[*, *, 1] = 1 - ((data and 11) gt 0)  ; green
    mdata[*, *, 2] = 1 - ((data and 19) gt 0)  ; red
    mdata[*, *, 3] = 1 - ((data and 35) gt 0)  ; red edge
    mdata[*, *, 4] = 1 - ((data and 67) gt 0)  ; NIR

    ; then apply the mask and make sure all data is positiv by clipping at zero
    openw, unit, out_name, /get_lun
    meta = envi_set_inheritance(fid_toc, dims, /full)
    for b = 0, nb - 1 do begin
      data = envi_get_data(fid = fid_toc, dims = dims, pos = b)
      data >= 0               ; make everything positive
      data *= mdata[*, *, b]  ; apply the masking
      writeu, unit, data
    endfor

    envi_setup_head, fname = out_name $
      , data_type = size(data, /type) $
      , ns = ns, nl = nl, nb = nb $
      , interleave = 0 $    ; 0 == BSQ
      , bnames = bnames $
      , /write $
      , inherit = meta

    ; close all files
    envi_file_mng, id = fid_inmask, /remove
    envi_file_mng, id = fid_toc, /remove
    envi_file_mng, id = fid_outmask, /remove
    close, unit
    free_lun, unit
  endfor


end
