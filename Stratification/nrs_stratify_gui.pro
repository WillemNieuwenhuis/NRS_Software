pro nrs_stratify_define_buttons, button_info
  compile_opt IDL2

  envi_define_menu_button, button_info, value = 'NDVI Stratification', $
    uvalue = 'View NDVI Stratification', event_pro = 'nrs_stratify_gui', $
    ref_value = 'NRS', position = 'last', /separator
end

pro nrs_stratify_extensions_init
  compile_opt idl2
  
  e = envi(/current)
  if e eq !NULL then return
  
  e.AddExtension, 'Stratification', 'nrs_stratify_gui', path='NRS'
end

pro nrs_stratify
  compile_opt idl2
end

pro nrs_stratify_gui_event, Event
  compile_opt IDL2

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  wWidget =  Event.top

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='nrs_stratify_gui_cancelbutton'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        widget_control, event.top, /destroy
    end

    else:
  endcase

end

pro nrs_stratify_draw_color, drawer, color, sel = sel
  compile_opt idl2
  
  widget_control, drawer, get_value = dispid
  wset, dispid
  erase, color = color
  
  if keyword_set(sel) then begin
    plots, [ [0.01, 0.01] $
        , [0.01, 0.95] $
        , [0.95, 0.95] $
        , [0.95, 0.01] $
        , [0.01, 0.01] ] $
        , /normal $
        , color = 0 $
        , /continue
  endif
end

pro nrs_stratify_gui, event
  compile_opt idl2

  statevalue={parent:       long(0), $
            matrix:         ptr_new(), $   ; stratification matrix
            mat_steps:      long(0), $     ; number of steps in Y direction
            fname:          string(''), $  ; name of listfile / datafile used to create stratification
            header:         string(''), $  ; comma separated list of fields names
            tracks:         string(''), $  ; comma separated list of track files
            start_year:     long(0), $     ; year of the (first) stack
            end_year:       long(0), $     ; year of the (last) stack
            line_data:      ptr_new(), $   ; the track data for the graphs
            line_color:     ptr_new(), $   ; array[10] with RGB color for each graph
            line_colix:     long(239), $   ; rotating index for selecting colors for graphs
            show_points:    long(1), $     ; draw track points (1) or not (0)
            show_line:      long(0), $     ; draw track line (1) or not (0)
            smooth_line:    long(0), $     ; Smooth line before drawing
            smooth_win:     long(3), $     ; window size of the smoothing filter
            dem_min:        float(0.0), $  ; min value for Y-axis
            dem_max:        float(0.0), $  ; max value for Y-axis
            x_min :         long(0), $     ; min value for X-axis
            x_max :         long(0), $     ; max value for X-axis
            draw_height:    long(0), $     ; total height of the plot area in pixels
            draw_width:     long(0), $     ; total width of the plot area in pixels
            legend_width:   long(0), $     ; total width of the legend plot area in pixels
            reserved_colors: long(16), $   ; reserved colors for tracks
            show_legend:    long(1), $     ; show (1) or hide(0) the legend box
            draw_scale:     float(0.0), $

            wSEED:0L}

  ; constants
  label_width = 100
  label_wide_width = 175
  text_width =  60
  text_small_width = 10
  num_width =   15
  
  ; dimensions of plot area
  scale = 1.5
  tot_height = 300 * scale
  tot_width = 600 * scale
  tot_leg_width = 50 * scale

  ; Form definition
  strat_form = widget_base( uname='stratification_form' $
               , /col $
               , tab_mode = 1 $
               , title = 'nrs_stratify')

  inputPanel = strat_form; widget_base(strat_form, /col)

  nrs_stratify_timeseries_panel = widget_base(inputPanel, /row)
  nrs_stratify_input_group = cw_groupbox(nrs_stratify_timeseries_panel, group_title = 'Time series')

  nrs_stratify_refstack = cw_dirfile(nrs_stratify_input_group $
                , title = 'Input time series' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stratify_refstack' $
                , event_pro = 'nrs_stratify_handle_input' $
              )

  nrs_stratify_start_year = fsc_inputfield(nrs_stratify_input_group $
                , uname = 'nrs_stratify_start_year' $
                , title = 'Start year' $
                , labelalign = 1 $
                , labelsize = label_width $
                , xsize = text_small_width $
                , /integer $
                , /all_events $
              )

  nrs_stratify_list = widget_text(nrs_stratify_timeseries_panel $
      , uname = 'nrs_stratify_list' $
      , ysize = 3 $
      , xsize = text_width $
      , value = '' $
    )

;  nrs_stratify_strat_group = cw_groupbox(inputPanel, group_title = 'Stratification options')
  nrs_stratify_strat_group = nrs_stratify_input_group
  nrs_stratify_calc_panel = widget_base(nrs_stratify_strat_group, /row)

  nrs_stratify_latitude_step = fsc_inputfield(nrs_stratify_calc_panel $
                , uname = 'nrs_stratify_latitude_step' $
                , title = 'Latitude steps' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '20' $
                , xsize = text_small_width $
                , /integer $
                , /all_events $
              )

  nrs_stratify_calcbutton = widget_button(nrs_stratify_calc_panel $
                , uname = 'nrs_stratify_calcbutton' $
                , value = 'Calculate' $
                , tooltip = 'Calculate the stratification' $
                , event_pro = 'nrs_stratify_calculate' $
    )

  nrs_stratify_track_group = cw_groupbox(inputPanel, group_title = 'Track options')
  nrs_stratify_table = cw_dirfile(nrs_stratify_track_group $
                , title = 'Track table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stratify_table' $
              )

  nrs_stratify_table_options1 = widget_base(nrs_stratify_track_group $
                , uname = 'nrs_stratify_table_options1' $
                , /row $
                  )

  nrs_stratify_track_method = cw_bgroup(nrs_stratify_table_options1 $
                , ['Replace displayed track(s)', 'Add to displayed track(s)'] $
                , /row, /exclusive $
                , set_value = 0 $
                , uname = 'nrs_stratify_track_method' $
                , ypad = 0 $
              ) 

  nrs_stratify_color_panel = widget_base(nrs_stratify_table_options1 $
                               , row = 1 $
                               , /frame $ 
                               , ypad = 0 $
                             )
                             
  draw_lab = widget_label(nrs_stratify_color_panel, value = 'Graph color ')
  draw_frame = widget_base(nrs_stratify_color_panel, /row)
  nrs_stratify_draw_id = widget_draw(draw_frame $ nrs_stratify_table_options1 $
                , XSize = 15, YSize = 15 $
                , /frame $
                , uname = 'nrs_stratify_draw_id' $
                , yoffset = 5 $
              )
              
  nrs_stratify_draw_button = widget_button(nrs_stratify_color_panel $
                , uname = 'nrs_stratify_draw_button' $
                , value = 'Colors' $
                , event_pro = 'nrs_stratify_color_select' $
              )

  nrs_stratify_display_track_button = widget_button(nrs_stratify_table_options1 $
                , value = 'Display track' $
                , uname = 'nrs_stratify_display_track_button' $
                , event_pro = 'nrs_stratify_display_track' $ 
              )

  nrs_stratify_table_options2 = widget_base(nrs_stratify_track_group $
                , uname = 'nrs_stratify_table_options2' $
                , /row $
                  )

  nrs_stratify_track_toggles = cw_bgroup(nrs_stratify_table_options2 $
                , ['Show observation points', ' Show line', 'Show track legend', 'Smooth'] $
                , /row, /nonexclusive $
                , set_value = 0 $
                , uname = 'nrs_stratify_track_toggles' $
                , event_funct = 'nrs_stratify_handle_track_toggles' $
                , ypad = -1 $
              )
              
  nrs_stratify_smooth_window = fsc_inputfield(nrs_stratify_table_options2 $
                , uname = 'nrs_stratify_smooth_window' $
                , title = 'Smoothing window' $
                , labelalign = 1 $
                , labelsize = label_width $
                , value = '3' $
                , xsize = text_small_width $
                , /integer $
                , /positiv $
                , /all_events $
                , event_pro = 'nrs_stratify_set_window' $
              )

    ; graph draw panel
  nrs_stratify_draw_content = widget_base(strat_form, /col)
  nrs_stratify_drawpanel = widget_base(nrs_stratify_draw_content, uname='nrs_stratify_drawpanel' ,frame=1  $
    , /col $
    ,title='IDL')

  nrs_stratify_panes = widget_base(nrs_stratify_drawpanel, uname='panes', /row, title = 'IDL')

  nrs_stratify_draw = widget_draw(nrs_stratify_panes, uname = 'nrs_stratify_draw' $
    , x_scroll_size = tot_width $
    , y_scroll_size = tot_height $
    , xsize = tot_width $
    , ysize = tot_height $
    , /app_scroll $
    , retain = 2 $
  )

  nrs_stratify_bottom_panel = widget_base(nrs_stratify_drawPanel, /row)

  nrs_stratify_lutTypePanel = widget_base(nrs_stratify_bottom_panel, /row $
  )

  lutDummyLabel = widget_label(nrs_stratify_lutTypePanel $
    , xsize = padding $
    , value = '' $
  )

  lutTypeLabel = widget_label(nrs_stratify_lutTypePanel, uname='lutTypeLabel' $
    , value = 'Select color table:' $
  )

  nrs_stratify_lutTypeCombobox = widget_combobox(nrs_stratify_lutTypePanel $
    , uname='nrs_stratify_lutTypeCombobox' $
    , value = ['Rainbow', 'Black/White', 'Custom'] $
    , event_pro = 'nrs_stratify_ChangeLutType' $
  )

  nrs_stratify_color_table = cw_dirfile(nrs_stratify_lutTypePanel $
                , title = 'Color table' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , sensitiv = 0 $
                , uname = 'nrs_stratify_color_table' $
                , event_pro = 'nrs_stratify_ChangeLutType' $
              )

  ; output panel
  nrs_stratify_output_group = cw_groupbox(inputPanel, group_title = 'Output')

  nrs_stratify_output_sv_panel = widget_base(nrs_stratify_output_group, /row)
  
  nrs_stratify_output_sv = widget_label(nrs_stratify_output_sv_panel $
                , uname = 'nrs_stratify_output_sv' $
                , value = 'Stratification values for observations' $
                , xsize = label_wide_width $
              )

  nrs_stratify_sv_button = widget_button(nrs_stratify_output_sv_panel $
                , uname = 'nrs_stratify_sv_button' $
                , value = 'Save' $
                , tooltip = 'Save stratification values for observations' $
                , event_pro = 'nrs_stratify_sv_tables' $
    )

  nrs_stratify_output_strat_panel = widget_base(nrs_stratify_output_group, /row)
  
  nrs_stratify_output_image = cw_dirfile(nrs_stratify_output_strat_panel $
                , title = 'Stratification name' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
                , uname = 'nrs_stratify_output_image' $
              )

  nrs_stratify_strat_savebutton = widget_button(nrs_stratify_output_strat_panel $
                , uname = 'nrs_stratify_strat_savebutton' $
                , value = 'Save' $
                , tooltip = 'Save the stratification to a file' $
                , event_pro = 'nrs_stratify_save_strat' $
    )
    
  nrs_stratify_output_pict_panel = widget_base(nrs_stratify_output_group, /row)
  
  nrs_stratify_output_JPG = cw_dirfile(nrs_stratify_output_pict_panel $
                , uname = 'nrs_stratify_output_JPG' $
                , title = 'Picture name' $
                , style = 'file' $
                , xsize = text_width $
                , xtitlesize = label_width $
              )

  nrs_stratify_savepicturebutton = widget_button(nrs_stratify_output_pict_panel $
                , uname = 'nrs_stratify_savepicturebutton' $
                , value = 'Save' $
                , tooltip = 'Save drawing as picture file' $
                , event_pro = 'nrs_stratify_SavePicture' $
    )

  statevalue.draw_height = tot_height / scale
  statevalue.draw_width = tot_width / scale
  statevalue.legend_width = tot_leg_width / scale
  statevalue.draw_scale = scale

  ; save the current form state ...
  widget_control, Strat_form, set_uvalue = statevalue

  nrs_gui_createButtonPanel, strat_form $
                , /no_ok $
                , cancel_uname = 'nrs_stratify_gui_cancelbutton', cancel_value = 'Done', cancel_tooltip = 'Cancel the operation'

  ; Make sure we create the form
  widget_control, /realize, strat_form

  ; Initialize stuff
  widget_control, nrs_stratify_track_toggles, set_value = [1, 0, 0, 0]
  device, decomposed = 0  ; we are using LUT colors

  XManager, 'nrs_stratify_gui', strat_form, /no_block

end
