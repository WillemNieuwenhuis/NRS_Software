pro _build_all_utils
  files = ['calc_permutation.pro' $
           , 'cgpickcolorname.pro' $
           , 'cgcolor24.pro' $
           , 'cgcentertlb.pro' $
           , 'cgrootname.pro' $
           , 'getprimaryscreensize.pro' $
;           , 'closeallresources.pro' $
           , 'cw_dirfile.pro' $
           , 'cw_field_ex.pro' $
           , 'cw_groupbox.pro' $
           , 'error_message.pro' $
           , 'nrs_get_extension.pro' $
           , 'fsc_color.pro' $
           , 'fsc_inputfield.pro' $
           , 'getoutname.pro' $
           , 'list_match.pro' $
           , 'mode.pro' $
           , 'nrs_ask_file.pro' $
           , 'nrs_assoc_cleanup.pro' $
           , 'nrs_calc_seq.pro' $
           , 'nrs_check_bounds.pro' $
           , 'nrs_dbf_utils.pro' $
           , 'nrs_find_images.pro' $
           , 'nrs_generate_coordmap.pro' $
           , 'nrs_get_days_indices.pro' $
           , 'nrs_gui_createbuttonpanel.pro' $
           , 'nrs_IDL_type_to_string.pro' $
           , 'nrs_julian_day_functions.pro' $
           , 'nrs_lin_regress.pro' $
           , 'nrs_load_lcb.pro' $
           , 'nrs_logging.pro' $
           , 'nrs_nice_numbers.pro' $
           , 'nrs_parse_modis_name.pro' $
           , 'nrs_read_listfile.pro' $
           , 'nrs_read_table.pro' $
           , 'nrs_sec_to_string.pro' $
           , 'nrs_shapefile_utils.pro' $
           , 'nrs_update_progress.pro' $
           , 'pickcolorname.pro' $
           , 'progressbar__define.pro' $
           , 'read_ini_file.pro' $
           , 'repchr.pro' $
           , 'setintersection.pro' $
           , 'sof.pro' $
           , 'sort_nd.pro' $
           , 'sourcepath.pro' $
           , 'widgetfont.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_utils.sav',logfile = 'bin' + path_sep() + 'nrs_utils.log'
  
  files = ['nrsmenu.pro']
  _auto_build, files, 'bin' + path_sep() + '_nrsmenu.sav', /no_lib, logfile = 'bin' + path_sep() + 'nrs_menu.log'

  files = ['nrs_show_routines.pro']
  _auto_build, files, 'bin' + path_sep() + 'nrs_routines_in_sav_gui.sav', /no_lib, logfile = 'bin' + path_sep() + 'nrs_routines_in_sav_gui.log'
end