!t_ctl_data_pvr_colormap.f90
!      module t_ctl_data_pvr_colormap
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine dealloc_pvr_color_crl(color)
!!
!!      subroutine read_control_data_colormap(color)
!!
!!      subroutine read_lighting_ctl(color)
!!      subroutine read_pvr_colordef_ctl(color)
!!      subroutine reset_pvr_colormap_flags(color)
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
      use t_control_data_pvr_sect
      use skip_comment_f
!
      implicit  none
!
!
!
      type pvr_colormap_ctl
!>  file name for modelves matrix
        character(len=kchara) :: color_file_ctl
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
!     2nd level for color definition
!
        integer (kind=kint) :: i_pvr_lighting =        0
        integer (kind=kint) :: i_pvr_colordef =        0
      end type pvr_colormap_ctl
!
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara) :: hd_pvr_colordef =  'pvr_color_ctl'
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
!
      private :: hd_pvr_lighting, hd_ambient, hd_diffuse, hd_specular
      private :: hd_light_param
      private :: hd_pvr_colordef, hd_colormap, hd_data_mapping
      private :: hd_pvr_range_min, hd_pvr_range_max
      private :: hd_colortable, hd_opacity_style
      private :: hd_constant_opacity, hd_opacity_def, hd_linear_opacity
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_color_crl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      call dealloc_control_array_r3(color%step_opacity_ctl)
      call dealloc_control_array_r2(color%linear_opacity_ctl)
      call dealloc_control_array_r3(color%light_position_ctl)
      call dealloc_control_array_r2(color%colortbl_ctl)
!
      color%colortbl_ctl%num =       0
      color%colortbl_ctl%icou = 0
      color%step_opacity_ctl%num =   0
      color%step_opacity_ctl%icou =    0
      color%linear_opacity_ctl%num = 0
      color%linear_opacity_ctl%icou =  0
      color%light_position_ctl%num = 0
      color%light_position_ctl%icou = 0
!
      end subroutine dealloc_pvr_color_crl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_colormap(color)
!
      use calypso_mpi
      use m_error_IDs
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
      call load_ctl_label_and_line
!
      if(right_begin_flag(hd_pvr_colordef) .gt. 0) then
        call read_pvr_colordef_ctl(color)
      else
        call calypso_mpi_abort(ierr_PVR, 'Set correct colormap file')
      end if
!
      end subroutine read_control_data_colormap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_lighting_ctl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      if(right_begin_flag(hd_pvr_lighting) .eq. 0) return
      if (color%i_pvr_lighting.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_lighting, color%i_pvr_lighting)
        if(color%i_pvr_lighting .gt. 0) exit
!
        call read_control_array_r3                                      &
     &     (hd_light_param, color%light_position_ctl)
!
        call read_real_ctl_type(hd_ambient, color%ambient_coef_ctl )
        call read_real_ctl_type(hd_diffuse, color%diffuse_coef_ctl )
        call read_real_ctl_type(hd_specular, color%specular_coef_ctl)
      end do
!
      end subroutine read_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colordef_ctl(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
      if (color%i_pvr_colordef.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_colordef, color%i_pvr_colordef)
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
!
        call read_chara_ctl_type(hd_colormap, color%colormap_ctl)
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
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_colormap_flags(color)
!
      type(pvr_colormap_ctl), intent(inout) :: color
!
!
!
      color%ambient_coef_ctl%iflag =  0
      color%diffuse_coef_ctl%iflag =  0
      color%specular_coef_ctl%iflag = 0
!
      color%colormap_ctl%iflag =        0
      color%data_mapping_ctl%iflag =    0
      color%range_min_ctl%iflag =       0
      color%range_max_ctl%iflag =       0
      color%opacity_style_ctl%iflag =   0
      color%fix_opacity_ctl%iflag =     0
!
      color%i_pvr_lighting = 0
      color%i_pvr_colordef = 0
!
      end subroutine reset_pvr_colormap_flags
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_colormap
