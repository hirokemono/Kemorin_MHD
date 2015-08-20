!m_control_data_4_pvr.f90
!      module m_control_data_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine deallocate_cont_dat_pvr(pvr)
!
!      subroutine read_control_data_pvr(pvr)
!      subroutine read_control_data_colormap(pvr)
!      subroutine read_vr_psf_ctl(pvr)
!      subroutine reset_pvr_control_flags
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  pvr_file_head        pvr_temp
!!  pvr_output_type      PNG
!!  image_tranceparency  tranceparent
!!!
!!  output_field    temperature    end
!!  output component     scalar
!!!
!!  begin plot_area_ctl
!!    array chosen_ele_grp_ctl  1
!!      chosen_ele_grp_ctl   outer_core   end
!!    end array chosen_ele_grp_ctl
!!  end  plot_area_ctl
!!!
!!  begin view_transform_ctl
!!    begin image_size_ctl
!!      x_pixel_ctl   1600
!!      y_pixel_ctl   1280
!!    end image_size_ctl
!!!
!!    array viewpoint_in_viewer_ctl  3
!!      viewpoint_in_viewer_ctl   x   0.000000000000e+00 end
!!      viewpoint_in_viewer_ctl   y   0.000000000000e+00 end
!!      viewpoint_in_viewer_ctl   z   3.474078182490e+01 end
!!    end array viewpoint_in_viewer_ctl
!!!
!!    scale_factor_ctl      1.000000000000e+00
!!!
!!    array look_at_point_ctl  3
!!      look_at_point_ctl         x   0.000000000000e+00 end
!!      look_at_point_ctl         y   0.000000000000e+00 end
!!      look_at_point_ctl         z   4.285221734758e-18 end
!!    end array look_at_point_ctl
!!!
!!    array view_rotation_vec_ctl  3
!!      view_rotation_vec_ctl     x   -3.100636350464e-01 end
!!      view_rotation_vec_ctl     y   -3.781962193178e-01 end
!!      view_rotation_vec_ctl     z   -8.721408327339e-01 end
!!    end array view_rotation_vec_ctl
!!    view_rotation_deg_ctl   1.040465377125e+02 end
!!!
!!    begin projection_matrix_ctl
!!      perspective_angle_ctl      9.765981906004e+00 end
!!      perspective_xy_ratio_ctl   1.250000000000e+00 end
!!      perspective_near_ctl       2.000000000000e-02 end
!!      perspective_far_ctl        1.000000000000e+03 end
!!    end projection_matrix_ctl
!!
!!      array modelview_matrix_ctl   16
!!        modelview_matrix_ctl   1  1  1.0  end
!!        modelview_matrix_ctl   2  1  0.0  end
!!        modelview_matrix_ctl   3  1  0.0  end
!!        modelview_matrix_ctl   4  1  0.0  end
!!!
!!        modelview_matrix_ctl   1  2  0.0  end
!!        modelview_matrix_ctl   2  2  1.0  end
!!        modelview_matrix_ctl   3  2  0.0  end
!!        modelview_matrix_ctl   4  2  0.0  end
!!
!!        modelview_matrix_ctl   1  3  0.0  end
!!        modelview_matrix_ctl   2  3  0.0  end
!!        modelview_matrix_ctl   3  3  1.0  end
!!        modelview_matrix_ctl   4  3  0.0  end
!!!
!!        modelview_matrix_ctl   1  4  0.0  end
!!        modelview_matrix_ctl   2  4  0.0  end
!!        modelview_matrix_ctl   3  4  0.0  end
!!        modelview_matrix_ctl   4  4  1.0  end
!!      end array modelview_matrix_ctl
!!  end view_transform_ctl
!!
!!  begin lighting_ctl
!!    array position_of_lights    4
!!      position_of_lights    0.0   0.0    0.0   end
!!      position_of_lights  -10.0   0.0  -10.0   end
!!      position_of_lights  -10.0   0.0    0.0   end
!!      position_of_lights    0.0  10.0    0.0   end
!!    end array position_of_lights
!!!
!!    ambient_coef              0.5
!!    diffuse_coef              5.6
!!    specular_coef             0.8
!!  end lighting_ctl
!!
!!  begin pvr_color_ctl
!!    colormap_ctl       rainbow
!!!
!!    data_mapping_ctl   Colormap_list
!!    array color_table_ctl    3
!!      color_table_ctl    0.0   0.0
!!      color_table_ctl    0.5   0.5
!!      color_table_ctl    1.0   1.0
!!    end array color_table_ctl
!!!
!!    opacity_style_ctl              point_linear
!!    array  linear_opacity_ctl         7
!!      linear_opacity_ctl   0.0     0.01
!!      linear_opacity_ctl   0.01    0.015
!!      linear_opacity_ctl   0.2     0.02
!!      linear_opacity_ctl   0.6     0.04
!!      linear_opacity_ctl   0.7     0.03
!!      linear_opacity_ctl   0.85    0.01
!!      linear_opacity_ctl   0.95    0.001
!!    end array linear_opacity_ctl
!!
!!    array  step_opacity_ctl         7
!!      step_opacity_ctl   0.0     0.01    0.01
!!      step_opacity_ctl   0.01    0.2     0.015
!!      step_opacity_ctl   0.2     0.35    0.02
!!      step_opacity_ctl   0.6     0.7     0.04
!!      step_opacity_ctl   0.7     0.85    0.03
!!      step_opacity_ctl   0.85    0.95    0.01
!!      step_opacity_ctl   0.95    1.0     0.001
!!    end array step_opacity_ctl
!!    constant_opacity_ctl           0.003
!!!
!!    range_min_ctl   0.0
!!    range_max_ctl   1.0
!!  end   pvr_color_ctl
!!!
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!!    font_size_ctl         3
!!    num_grid_ctl     4
!!  end colorbar_ctl
!!!
!!  begin image_rotation_ctl
!!    hd_movie_rot_axis       z
!!    num_of_frames           1
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_4_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use m_ctl_data_4_view_transfer
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_ctl
!>  file name for modelves matrix
        character(len=kchara) :: view_file_ctl
!>  file name for modelves matrix
        character(len=kchara) :: color_file_ctl
!
!>    Structure for modelview marices
        type(modeview_ctl) :: mat
!
        type(read_character_item) :: file_head_ctl
        type(read_character_item) :: file_fmt_ctl
        type(read_character_item) :: transparent_ctl
!
!>      Structure for element group list for PVR
!!@n      group_4_monitor_ctl%c_tbl: Name of element group for PVR
        type(ctl_array_chara) :: pvr_area_ctl
!
        character(len=kchara) :: pvr_field_ctl(1)
        character(len=kchara) :: pvr_comp_ctl(1)
!
!
        type(read_real_item) :: ambient_coef_ctl
        type(read_real_item) :: diffuse_coef_ctl
        type(read_real_item) :: specular_coef_ctl
!
!>      Structure for light positions
!!@n      light_position_ctl%vec1:  X-component of light position
!!@n      light_position_ctl%vec2:  Y-component of light position
!!@n      light_position_ctl%vec3:  Z-component of light position
        type(ctl_array_r3) :: light_position_ctl
!
        type(read_character_item) :: rotation_axis_ctl
        type(read_integer_item) ::   num_frames_ctl
!
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
        type(read_character_item) :: colormap_ctl
        type(read_character_item) :: data_mapping_ctl
        type(read_character_item) :: opacity_style_ctl
!
        type(read_real_item) :: range_min_ctl
        type(read_real_item) :: range_max_ctl
        type(read_real_item) :: fix_opacity_ctl
!
!>      Structure for color map controls
!!@n      colortbl_ctl%vec1:  field data value
!!@n      colortbl_ctl%vec2:  color map value
        type(ctl_array_r2) :: colortbl_ctl
!
!>        Structure for opacity controls
!!@n        linear_opacity_ctl%vec1:  field value to define opacity
!!@n        linear_opacity_ctl%vec3:  Opacity at this point
        type(ctl_array_r2) :: linear_opacity_ctl
!>        Structure for opacity controls
!!@n        step_opacity_ctl%vec1:  Minimum value for one opacity
!!@n        step_opacity_ctl%vec2:  Maximum value for one opacity
!!@n        step_opacity_ctl%vec3:  Opacity for each level
        type(ctl_array_r3) :: step_opacity_ctl
!
!
!     Top level
!
        integer (kind=kint) :: i_pvr_ctl = 0
!
        integer (kind=kint) :: i_view_file =  0
        integer (kind=kint) :: i_color_file = 0
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_output_field_def =    0
        integer (kind=kint) :: i_output_comp_def =     0
        integer (kind=kint) :: i_plot_area =           0
!
        integer (kind=kint) :: i_pvr_lighting =        0
        integer (kind=kint) :: i_pvr_colordef =        0
        integer (kind=kint) :: i_pvr_colorbar =        0
        integer (kind=kint) :: i_pvr_rotation =        0
      end type pvr_ctl
!
!
!     Top level
!
      character(len=kchara) :: hd_vr_psf_ctl = 'volume_rendering'
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_file_head =     'pvr_file_head'
      character(len=kchara) :: hd_pvr_out_type =      'pvr_output_type'
      character(len=kchara) :: hd_pvr_rgba_type = 'image_tranceparency'
      character(len=kchara) :: hd_output_field_def = 'output_field'
      character(len=kchara) :: hd_output_comp_def =  'output_component'
!
      character(len=kchara) :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_pvr_rotation =  'image_rotation_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara) :: hd_plot_area =   'plot_area_ctl'
!
!     4th level for plot_area
!
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
!
!     3rd level for lighting
!
      character(len=kchara) :: hd_ambient =  'ambient_coef_ctl'
      character(len=kchara) :: hd_diffuse =  'diffuse_coef_ctl'
      character(len=kchara) :: hd_specular = 'specular_coef_ctl'
      character(len=kchara) :: hd_light_param =  'position_of_lights'
!
!     3rd level for colormap
!
      character(len=kchara) :: hd_colormap =     'colormap_ctl'
      character(len=kchara) :: hd_data_mapping = 'data_mapping_ctl'
      character(len=kchara) :: hd_pvr_range_min = 'range_min_ctl'
      character(len=kchara) :: hd_pvr_range_max = 'range_max_ctl'
      character(len=kchara) :: hd_colortable = 'color_table_ctl'
      character(len=kchara) :: hd_opacity_style = 'opacity_style_ctl'
      character(len=kchara) :: hd_constant_opacity                      &
     &                        = 'constant_opacity_ctl'
      character(len=kchara) :: hd_linear_opacity = 'linear_opacity_ctl'
      character(len=kchara) :: hd_opacity_def =    'step_opacity_ctl'
!
!     3rd level for colorbar
!
      character(len=kchara)                                             &
     &                    :: hd_colorbar_switch = 'colorbar_switch_ctl'
      character(len=kchara) :: hd_colorbar_scale = 'colorbar_scale_ctl'
      character(len=kchara) :: hd_pvr_font_size = 'font_size_ctl'
      character(len=kchara) :: hd_pvr_numgrid_cbar = 'num_grid_ctl'
      character(len=kchara) :: hd_zeromarker_flag = 'iflag_zeromarker'
      character(len=kchara) :: hd_cbar_range = 'colorbar_range'
!
!     3rd level for rotation
!
      character(len=kchara) :: hd_movie_rot_axis =  'rotation_axis_ctl'
      character(len=kchara) :: hd_movie_rot_frame = 'num_frames_ctl'
!
      private :: hd_pvr_file_head, hd_pvr_out_type, hd_pvr_rgba_type
      private :: hd_output_field_def
      private :: hd_plot_area, hd_output_comp_def
      private :: hd_plot_grp
      private :: hd_pvr_lighting, hd_ambient, hd_diffuse, hd_specular
      private :: hd_light_param, hd_pvr_colorbar, hd_pvr_rotation
      private :: hd_pvr_colordef, hd_colormap, hd_data_mapping
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
      private :: hd_pvr_range_min, hd_pvr_range_max
      private :: hd_colortable, hd_opacity_style
      private :: hd_constant_opacity, hd_opacity_def, hd_linear_opacity
!
      private :: read_plot_area_ctl
      private :: read_lighting_ctl
      private :: read_pvr_rotation_ctl
      private :: read_pvr_colorbar_ctl, read_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_cont_dat_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(pvr%step_opacity_ctl%num .gt. 0) then
        call dealloc_control_array_r3(pvr%step_opacity_ctl)
      end if
      if(pvr%linear_opacity_ctl%num .gt. 0) then
        call dealloc_control_array_r2(pvr%linear_opacity_ctl)
      end if
      if(pvr%light_position_ctl%num .gt. 0) then
        call dealloc_control_array_r3(pvr%light_position_ctl)
      end if
!
      if(pvr%colortbl_ctl%num .gt. 0) then
        call dealloc_control_array_r2(pvr%colortbl_ctl)
      end if
!
      call dealloc_control_array_chara(pvr%pvr_area_ctl)
!
      end subroutine deallocate_cont_dat_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_pvr(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      call load_ctl_label_and_line
!
      call read_vr_psf_ctl(pvr)
!
      end subroutine read_control_data_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_colormap(pvr)
!
      use calypso_mpi
      use m_error_IDs
!
      type(pvr_ctl), intent(inout) :: pvr
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
        call read_pvr_colordef_ctl(pvr)
      else
        call calypso_mpi_abort(ierr_PVR, 'Set correct colormap file')
      end if
!
      end subroutine read_control_data_colormap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_vr_psf_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_vr_psf_ctl) .eq. 0) return
      if (pvr%i_pvr_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_vr_psf_ctl, pvr%i_pvr_ctl)
        if(pvr%i_pvr_ctl .gt. 0) exit
!
!
        if(right_file_flag(hd_view_transform) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_view_file,            &
     &        pvr%view_file_ctl)
        else if(right_begin_flag(hd_view_transform) .gt. 0) then
          pvr%view_file_ctl = 'NO_FILE'
          call read_view_transfer_ctl(pvr%mat)
        end if
!
        if(right_file_flag(hd_pvr_colordef) .gt. 0) then
          call read_file_name_from_ctl_line(pvr%i_color_file,           &
     &        pvr%color_file_ctl)
        else if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
          pvr%color_file_ctl = 'NO_FILE'
          call read_pvr_colordef_ctl(pvr)
        end if
!
        call read_plot_area_ctl(pvr)
        call read_lighting_ctl(pvr)
        call read_pvr_colorbar_ctl(pvr)
        call read_pvr_rotation_ctl(pvr)
!
!
        call read_chara_ctl_type(hd_pvr_file_head, pvr%file_head_ctl)
        call read_chara_ctl_type(hd_pvr_out_type,  pvr%file_fmt_ctl )
        call read_chara_ctl_type(hd_pvr_rgba_type, pvr%transparent_ctl)
        call read_character_ctl_item(hd_output_field_def,               &
     &          pvr%i_output_field_def, pvr%pvr_field_ctl(1) )
        call read_character_ctl_item(hd_output_comp_def,                &
     &          pvr%i_output_comp_def, pvr%pvr_comp_ctl(1) )
      end do
!
      end subroutine read_vr_psf_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_plot_area_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_plot_area) .eq. 0) return
      if (pvr%i_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_plot_area, pvr%i_plot_area)
        if(pvr%i_plot_area .gt. 0) exit
!
        call read_control_array_c1(hd_plot_grp, pvr%pvr_area_ctl)
      end do
!
      end subroutine read_plot_area_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_lighting_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_pvr_lighting) .eq. 0) return
      if (pvr%i_pvr_lighting.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_lighting, pvr%i_pvr_lighting)
        if(pvr%i_pvr_lighting .gt. 0) exit
!
        call read_control_array_r3                                      &
     &     (hd_light_param, pvr%light_position_ctl)
!
        call read_real_ctl_type(hd_ambient, pvr%ambient_coef_ctl )
        call read_real_ctl_type(hd_diffuse, pvr%diffuse_coef_ctl )
        call read_real_ctl_type(hd_specular, pvr%specular_coef_ctl)
      end do
!
      end subroutine read_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colordef_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if (pvr%i_pvr_colordef.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_colordef, pvr%i_pvr_colordef)
        if(pvr%i_pvr_colordef .gt. 0) exit
!
!
        call read_control_array_r2(hd_colortable, pvr%colortbl_ctl)
        call read_control_array_r2                                      &
     &     (hd_linear_opacity, pvr%linear_opacity_ctl)
!
        call read_control_array_r3                                      &
     &     (hd_opacity_def, pvr%step_opacity_ctl)
!
!
        call read_chara_ctl_type(hd_colormap, pvr%colormap_ctl)
        call read_chara_ctl_type(hd_data_mapping, pvr%data_mapping_ctl)
        call read_chara_ctl_type(hd_opacity_style,                      &
     &      pvr%opacity_style_ctl)
!
        call read_real_ctl_type(hd_pvr_range_min, pvr%range_min_ctl)
        call read_real_ctl_type(hd_pvr_range_max, pvr%range_max_ctl)
        call read_real_ctl_type(hd_constant_opacity,                    &
     &      pvr%fix_opacity_ctl)
      end do
!
      end subroutine read_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_pvr_colorbar) .eq. 0) return
      if (pvr%i_pvr_colorbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_colorbar, pvr%i_pvr_colorbar)
        if(pvr%i_pvr_colorbar .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_pvr_font_size, pvr%font_size_ctl)
        call read_integer_ctl_type(hd_pvr_numgrid_cbar,                 &
     &      pvr%ngrid_cbar_ctl )
!
!
        call read_chara_ctl_type(hd_colorbar_switch,                    &
     &      pvr%colorbar_switch_ctl )
        call read_chara_ctl_type(hd_colorbar_scale,                     &
     &      pvr%colorbar_scale_ctl )
        call read_chara_ctl_type(hd_zeromarker_flag,                    &
     &      pvr%zeromarker_flag_ctl )
!
        call read_real2_ctl_type(hd_cbar_range, pvr%cbar_range_ctl )
      end do
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      if(right_begin_flag(hd_pvr_rotation) .eq. 0) return
      if (pvr%i_pvr_rotation.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_rotation, pvr%i_pvr_rotation)
        if(pvr%i_pvr_rotation .gt. 0) exit
!
        call read_integer_ctl_type(hd_movie_rot_frame,                  &
     &      pvr%num_frames_ctl )
        call read_chara_ctl_type(hd_movie_rot_axis,                     &
     &      pvr%rotation_axis_ctl )
      end do
!
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_control_flags(pvr)
!
      type(pvr_ctl), intent(inout) :: pvr
!
!
      pvr%pvr_area_ctl%num =       0
      pvr%light_position_ctl%num = 0
      pvr%colortbl_ctl%num =       0
      pvr%step_opacity_ctl%num =   0
      pvr%linear_opacity_ctl%num = 0
!
      pvr%file_head_ctl%iflag =   0
      pvr%file_fmt_ctl%iflag =    0
      pvr%transparent_ctl%iflag = 0
      pvr%i_output_field_def =    0
      pvr%i_output_comp_def =     0
      pvr%ambient_coef_ctl%iflag =  0
      pvr%diffuse_coef_ctl%iflag =  0
      pvr%specular_coef_ctl%iflag = 0
      pvr%light_position_ctl%icou = 0
      pvr%num_frames_ctl%iflag =    0
      pvr%rotation_axis_ctl%iflag = 0
!
      pvr%colorbar_switch_ctl%iflag = 0
      pvr%colorbar_scale_ctl%iflag =  0
      pvr%font_size_ctl%iflag =       0
      pvr%ngrid_cbar_ctl%iflag =      0
      pvr%zeromarker_flag_ctl%iflag = 0
      pvr%cbar_range_ctl%iflag =      0
      pvr%colormap_ctl%iflag =        0
      pvr%data_mapping_ctl%iflag =    0
      pvr%range_min_ctl%iflag =       0
      pvr%range_max_ctl%iflag =       0
      pvr%opacity_style_ctl%iflag =   0
      pvr%fix_opacity_ctl%iflag =     0
!
      pvr%i_pvr_ctl = 0
      pvr%i_plot_area =   0
      pvr%pvr_area_ctl%icou = 0
!
      pvr%i_pvr_lighting = 0
!
      pvr%i_pvr_rotation = 0
      pvr%i_pvr_colorbar = 0
!
      pvr%i_pvr_colordef = 0
      pvr%colortbl_ctl%icou = 0
      pvr%step_opacity_ctl%icou =    0
      pvr%linear_opacity_ctl%icou =  0
!
      call reset_view_transfer_ctl(pvr%mat)
!
      end subroutine reset_pvr_control_flags
!
!  ---------------------------------------------------------------------
!
      end module m_control_data_4_pvr
