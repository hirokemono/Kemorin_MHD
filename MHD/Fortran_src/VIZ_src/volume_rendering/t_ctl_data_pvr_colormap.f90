!t_ctl_data_pvr_colormap.f90
!      module t_ctl_data_pvr_colormap
!
!        programmed by H.Matsui on May. 2006
!!
!!      subroutine deallocate_pvr_cmap_cbar(cmap_cbar_c)
!!      subroutine read_pvr_cmap_cbar(hd_block, cmap_cbar_c)
!!        type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!!
!!      subroutine dealloc_pvr_light_crl(light)
!!      subroutine read_lighting_ctl(hd_block, light)
!!      subroutine reset_pvr_light_flags(light)
!!        type(pvr_light_ctl), intent(inout) :: light
!!
!!      subroutine dealloc_pvr_color_crl(color)
!!      subroutine read_pvr_colordef_ctl(hd_block, color)
!!      subroutine reset_pvr_colormap_flags(color)
!!        type(pvr_colormap_ctl), intent(inout) :: color
!!
!!      subroutine read_pvr_colorbar_ctl(hd_block, cbar_ctl)
!!      subroutine reset_pvr_colorbar_ctl_flags(cbar_ctl)
!!        type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  begin lighting_ctl
!!    array position_of_lights    4
!!      position_of_lights    0.0   0.0    0.0   end
!!      position_of_lights  -10.0   0.0  -10.0   end
!!      position_of_lights  -10.0   0.0    0.0   end
!!      position_of_lights    0.0  10.0    0.0   end
!!    end array position_of_lights
!!!
!!    ambient_coef_ctl              0.5
!!    diffuse_coef_ctl              5.6
!!    specular_coef_ctl             0.8
!!  end lighting_ctl
!!
!!  begin colormap_ctl
!!    colormap_mode_ctl       rainbow
!!!
!!    LIC_color_field             magnetic_field
!!    LIC_color_componenet        magnitude
!!
!!    LIC_transparent_field         magnetic_field
!!    LIC_transparent_componenet    magnitude
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
!!  end   colormap_ctl
!!
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!    font_size_ctl         3
!!    num_grid_ctl     4
!!!
!!    axis_label_switch      ON
!!  end colorbar_ctl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_ctl_data_pvr_colormap
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
      use bcast_control_arrays
!
      implicit  none
!
!
      type pvr_light_ctl
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
        integer (kind=kint) :: i_pvr_lighting = 0
      end type pvr_light_ctl
!
!
      type pvr_colormap_ctl
        type(read_character_item) :: lic_color_fld_ctl
        type(read_character_item) :: lic_color_comp_ctl
        type(read_character_item) :: lic_opacity_fld_ctl
        type(read_character_item) :: lic_opacity_comp_ctl
!
        type(read_character_item) :: colormap_mode_ctl
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
!     2nd level for color definition
        integer (kind=kint) :: i_pvr_colordef =        0
      end type pvr_colormap_ctl
!
!
      type pvr_colorbar_ctl
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
        type(read_character_item) :: axis_switch_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_colorbar = 0
      end type pvr_colorbar_ctl
!
!
!>  Structure of control data for PVR colormap and colorbar
      type pvr_colormap_bar_ctl
!>    Structure for colormap
        type(pvr_colormap_ctl) :: color
!>    Structure for colorbar
        type(pvr_colorbar_ctl) :: cbar_ctl
!
        integer (kind=kint) :: i_cmap_cbar = 0
      end type pvr_colormap_bar_ctl
!
!
!     2nd level for colormap and colorbar
!
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_colormap =      'colormap_ctl'
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
      character(len=kchara) :: hd_colormap_mode =  'colormap_mode_ctl'
!
      character(len=kchara)                                             &
     &        :: hd_lic_color_fld =  'LIC_color_field'
      character(len=kchara)                                             &
     &        :: hd_lic_color_comp =  'LIC_color_componenet'
      character(len=kchara)                                             &
     &        :: hd_lic_opacity_fld =  'LIC_transparent_field'
      character(len=kchara)                                             &
     &        :: hd_lic_opacity_comp =  'LIC_transparent_componenet'
!
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
      character(len=kchara) :: hd_axis_switch = 'axis_label_switch'
!
      private :: hd_ambient, hd_diffuse, hd_specular
      private :: hd_light_param, hd_colormap_mode, hd_data_mapping
      private :: hd_pvr_range_min, hd_pvr_range_max
      private :: hd_colortable, hd_opacity_style
      private :: hd_constant_opacity, hd_opacity_def, hd_linear_opacity
!
      private :: hd_colorbar_switch, hd_colorbar_scale
      private :: hd_pvr_font_size, hd_cbar_range
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
      private :: hd_axis_switch
      private :: hd_colormap, hd_pvr_colorbar
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_cmap_cbar(cmap_cbar_c)
!
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!
      call reset_pvr_colormap_flags(cmap_cbar_c%color)
      call reset_pvr_colorbar_ctl_flags(cmap_cbar_c%cbar_ctl)
      call dealloc_pvr_color_crl(cmap_cbar_c%color)
!
      cmap_cbar_c%i_cmap_cbar = 0
!
      end subroutine deallocate_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_cmap_cbar(hd_block, cmap_cbar_c)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_colormap_bar_ctl), intent(inout) :: cmap_cbar_c
!
!
      if (cmap_cbar_c%i_cmap_cbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        cmap_cbar_c%i_cmap_cbar = find_control_end_flag(hd_block)
        if(cmap_cbar_c%i_cmap_cbar .gt. 0) exit
!
        call read_pvr_colordef_ctl(hd_colormap, cmap_cbar_c%color)
        call read_pvr_colorbar_ctl                                      &
     &     (hd_pvr_colorbar, cmap_cbar_c%cbar_ctl)
      end do
!
      end subroutine read_pvr_cmap_cbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_light_crl(light)
!
      type(pvr_light_ctl), intent(inout) :: light
!
      call dealloc_control_array_r3(light%light_position_ctl)
      light%light_position_ctl%num = 0
      light%light_position_ctl%icou = 0
!
      end subroutine dealloc_pvr_light_crl
!
!  ---------------------------------------------------------------------
!
      subroutine read_lighting_ctl(hd_block, light)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(inout) :: light
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (light%i_pvr_lighting.gt.0) return
      do
        call load_ctl_label_and_line
!
        light%i_pvr_lighting = find_control_end_flag(hd_block)
        if(light%i_pvr_lighting .gt. 0) exit
!
        call read_control_array_r3                                      &
     &     (hd_light_param, light%light_position_ctl)
!
        call read_real_ctl_type(hd_ambient, light%ambient_coef_ctl )
        call read_real_ctl_type(hd_diffuse, light%diffuse_coef_ctl )
        call read_real_ctl_type(hd_specular, light%specular_coef_ctl)
      end do
!
      end subroutine read_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_light_flags(light)
!
      type(pvr_light_ctl), intent(inout) :: light
!
!
      light%ambient_coef_ctl%iflag =  0
      light%diffuse_coef_ctl%iflag =  0
      light%specular_coef_ctl%iflag = 0
      light%i_pvr_lighting = 0
!
      end subroutine reset_pvr_light_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_crl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call dealloc_control_array_r3(color%step_opacity_ctl)
      call dealloc_control_array_r2(color%linear_opacity_ctl)
      call dealloc_control_array_r2(color%colortbl_ctl)
!
      color%colortbl_ctl%num =       0
      color%colortbl_ctl%icou = 0
      color%step_opacity_ctl%num =   0
      color%step_opacity_ctl%icou =    0
      color%linear_opacity_ctl%num = 0
      color%linear_opacity_ctl%icou =  0
!
      end subroutine dealloc_pvr_color_crl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colordef_ctl(hd_block, color)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if(color%i_pvr_colordef.gt.0) return
      do
        call load_ctl_label_and_line
!
        color%i_pvr_colordef = find_control_end_flag(hd_block)
        if(color%i_pvr_colordef .gt. 0) exit
!
!
        call read_control_array_r2(hd_colortable, color%colortbl_ctl)
        call read_control_array_r2                                      &
     &     (hd_linear_opacity, color%linear_opacity_ctl)
!
        call read_control_array_r3                                      &
     &     (hd_opacity_def, color%step_opacity_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_lic_color_fld, color%lic_color_fld_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_lic_color_comp, color%lic_color_comp_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_lic_opacity_fld, color%lic_opacity_fld_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_lic_opacity_comp, color%lic_opacity_comp_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_colormap_mode, color%colormap_mode_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_data_mapping, color%data_mapping_ctl)
        call read_chara_ctl_type(hd_opacity_style,                      &
     &      color%opacity_style_ctl)
!
        call read_real_ctl_type(hd_pvr_range_min, color%range_min_ctl)
        call read_real_ctl_type(hd_pvr_range_max, color%range_max_ctl)
        call read_real_ctl_type(hd_constant_opacity,                    &
     &      color%fix_opacity_ctl)
      end do
!
      end subroutine read_pvr_colordef_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_colormap_flags(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      color%lic_color_fld_ctl%iflag =    0
      color%lic_color_comp_ctl%iflag =   0
      color%lic_opacity_fld_ctl%iflag =  0
      color%lic_opacity_comp_ctl%iflag = 0
!
      color%colormap_mode_ctl%iflag =   0
      color%data_mapping_ctl%iflag =    0
      color%range_min_ctl%iflag =       0
      color%range_max_ctl%iflag =       0
      color%opacity_style_ctl%iflag =   0
      color%fix_opacity_ctl%iflag =     0
!
      color%i_pvr_colordef = 0
!
      end subroutine reset_pvr_colormap_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl(hd_block, cbar_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (cbar_ctl%i_pvr_colorbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        cbar_ctl%i_pvr_colorbar                                         &
     &      = find_control_end_flag(hd_block)
        if(cbar_ctl%i_pvr_colorbar .gt. 0) exit
!
!
        call read_integer_ctl_type                                      &
     &     (hd_pvr_font_size, cbar_ctl%font_size_ctl)
        call read_integer_ctl_type(hd_pvr_numgrid_cbar,                 &
     &      cbar_ctl%ngrid_cbar_ctl)
!
!
        call read_chara_ctl_type(hd_colorbar_switch,                    &
     &      cbar_ctl%colorbar_switch_ctl)
        call read_chara_ctl_type(hd_colorbar_scale,                     &
     &      cbar_ctl%colorbar_scale_ctl)
        call read_chara_ctl_type(hd_zeromarker_flag,                    &
     &      cbar_ctl%zeromarker_flag_ctl)
!
        call read_chara_ctl_type(hd_axis_switch,                        &
     &      cbar_ctl%axis_switch_ctl)
!!
        call read_real2_ctl_type                                        &
     &     (hd_cbar_range, cbar_ctl%cbar_range_ctl)
      end do
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_colorbar_ctl_flags(cbar_ctl)
!
      type(pvr_colorbar_ctl), intent(inout) :: cbar_ctl
!
!
      cbar_ctl%colorbar_switch_ctl%iflag = 0
      cbar_ctl%colorbar_scale_ctl%iflag =  0
      cbar_ctl%font_size_ctl%iflag =       0
      cbar_ctl%ngrid_cbar_ctl%iflag =      0
      cbar_ctl%zeromarker_flag_ctl%iflag = 0
      cbar_ctl%cbar_range_ctl%iflag =      0
!
      cbar_ctl%i_pvr_colorbar = 0
!
      end subroutine reset_pvr_colorbar_ctl_flags
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colormap
