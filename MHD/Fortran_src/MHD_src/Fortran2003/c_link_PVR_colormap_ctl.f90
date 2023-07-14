!>@file   c_link_PVR_colormap_ctl.f90
!!@brief  module c_link_PVR_colormap_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_PVR_light_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_light_ctl_block_name')
!!      type(c_ptr) function c_PVR_light_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_PVR_light_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_light_ambient_coef_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_light_ambient_coef_ctl')
!!      type(c_ptr) function c_PVR_light_diffuse_coef_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_light_diffuse_coef_ctl')
!!      type(c_ptr) function c_PVR_light_specular_coef_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_light_specular_coef_ctl')
!!      type(c_ptr) function c_PVR_light_position_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_light_position_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_PVR_colormap_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_colormap_ctl_block_name')
!!      type(c_ptr) function c_PVR_colormap_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_colormap_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_cmap_lic_color_comp_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_cmap_lic_color_comp_ctl')
!!      type(c_ptr) function c_PVR_cmap_lic_opacity_fld_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_cmap_lic_opacity_fld_ctl')
!!      type(c_ptr) function c_PVR_cmap_lic_opacity_comp_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_PVR_cmap_lic_opacity_comp_ctl')
!!      type(c_ptr) function c_PVR_cmap_colormap_mode_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_cmap_colormap_mode_ctl')
!!      type(c_ptr) function c_PVR_cmap_data_mapping_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_PVR_cmap_data_mapping_ctl')
!!      type(c_ptr) function c_PVR_cmap_opacity_style_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_PVR_cmap_opacity_style_ctl')
!!      type(c_ptr) function c_PVR_cmap_range_min_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_cmap_range_min_ctl')
!!      type(c_ptr) function c_PVR_cmap_range_max_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_cmap_range_max_ctl')
!!      type(c_ptr) function c_PVR_cmap_fix_opacity_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_cmap_fix_opacity_ctl')
!!      type(c_ptr) function c_PVR_cmap_colortbl_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_PVR_cmap_colortbl_ctl')
!!      type(c_ptr) function c_PVR_cmap_linear_opacity_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_cmap_linear_opacity_ctl')
!!      type(c_ptr) function c_PVR_cmap_step_opacity_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_PVR_cmap_step_opacity_ctl')
!!      type(c_ptr) function c_PVR_cmap_background_color_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_PVR_cmap_background_color_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_PVR_colormap_ctl
!
      use iso_c_binding
      use t_ctl_data_pvr_colormap
      use t_ctl_data_pvr_light
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_light_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_light_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_PVR_light_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_ctl_iflag = C_loc(f_ctl%i_pvr_lighting)
      end function c_PVR_light_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_ambient_coef_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_light_ambient_coef_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_ambient_coef_ctl = C_loc(f_ctl%ambient_coef_ctl)
      end function c_PVR_light_ambient_coef_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_diffuse_coef_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_light_diffuse_coef_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_diffuse_coef_ctl = C_loc(f_ctl%diffuse_coef_ctl)
      end function c_PVR_light_diffuse_coef_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_specular_coef_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_light_specular_coef_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_specular_coef_ctl = C_loc(f_ctl%specular_coef_ctl)
      end function c_PVR_light_specular_coef_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_light_position_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_light_position_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_light_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_light_position_ctl = C_loc(f_ctl%light_position_ctl)
      end function c_PVR_light_position_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colormap_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_colormap_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colormap_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_colormap_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_colormap_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_colormap_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_colormap_ctl_iflag = C_loc(f_ctl%i_pvr_colordef)
      end function c_PVR_colormap_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_lic_color_comp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_cmap_lic_color_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_lic_color_comp_ctl = C_loc(f_ctl%lic_color_comp_ctl)
      end function c_PVR_cmap_lic_color_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_lic_opacity_fld_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_cmap_lic_opacity_fld_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_lic_opacity_fld_ctl = C_loc(f_ctl%lic_opacity_fld_ctl)
      end function c_PVR_cmap_lic_opacity_fld_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_lic_opacity_comp_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_PVR_cmap_lic_opacity_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_lic_opacity_comp_ctl                                   &
     &            = C_loc(f_ctl%lic_opacity_comp_ctl)
      end function c_PVR_cmap_lic_opacity_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_colormap_mode_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_cmap_colormap_mode_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_colormap_mode_ctl = C_loc(f_ctl%colormap_mode_ctl)
      end function c_PVR_cmap_colormap_mode_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_data_mapping_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_PVR_cmap_data_mapping_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_data_mapping_ctl = C_loc(f_ctl%data_mapping_ctl)
      end function c_PVR_cmap_data_mapping_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_opacity_style_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_PVR_cmap_opacity_style_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_opacity_style_ctl = C_loc(f_ctl%opacity_style_ctl)
      end function c_PVR_cmap_opacity_style_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_range_min_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_cmap_range_min_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_range_min_ctl = C_loc(f_ctl%range_min_ctl)
      end function c_PVR_cmap_range_min_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_range_max_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_cmap_range_max_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_range_max_ctl = C_loc(f_ctl%range_max_ctl)
      end function c_PVR_cmap_range_max_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_fix_opacity_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_cmap_fix_opacity_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_fix_opacity_ctl = C_loc(f_ctl%fix_opacity_ctl)
      end function c_PVR_cmap_fix_opacity_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_colortbl_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_cmap_colortbl_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_colortbl_ctl = C_loc(f_ctl%colortbl_ctl)
      end function c_PVR_cmap_colortbl_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_linear_opacity_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_cmap_linear_opacity_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_linear_opacity_ctl = C_loc(f_ctl%linear_opacity_ctl)
      end function c_PVR_cmap_linear_opacity_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_step_opacity_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_PVR_cmap_step_opacity_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_step_opacity_ctl = C_loc(f_ctl%step_opacity_ctl)
      end function c_PVR_cmap_step_opacity_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_cmap_background_color_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_PVR_cmap_background_color_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_colormap_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_cmap_background_color_ctl                                   &
     &            = C_loc(f_ctl%background_color_ctl)
      end function c_PVR_cmap_background_color_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_PVR_colormap_ctl
