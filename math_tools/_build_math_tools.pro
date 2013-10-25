pro _build_math_tools
  files = ['nrs_math_define_buttons.pro' $
          , 'nrs_math_menu.pro' $
          , 'nrs_scale_offset.pro' $
          , 'nrs_scale_offset_gui.pro' $
          , 'nrs_scale_offset_gui_eventcb.pro' $
          ]
  _auto_build, files, 'bin' + path_sep() + 'nrs_math_tools.sav',logfile = 'bin' + path_sep() + 'nrs_math_tools.log'
end