!>@file   c_link_VIZ_PVR_ctl.f90
!!@brief  module c_link_VIZ_PVR_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_PVR_ctl_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_PVR_ctl_block_name')
!!      type(c_ptr) function c_VIZ_PVR_ctl_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_PVR_fname_mat_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_PVR_fname_mat_ctl')
!!      type(c_ptr) function c_VIZ_PVR_viewmat_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_PVR_viewmat_ctl')
!!      type(c_ptr) function c_VIZ_PVR_fname_pvr_light_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_PVR_fname_pvr_light_ctl')
!!      type(c_ptr) function c_VIZ_PVR_light_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_light_ctl')
!!      type(c_ptr) function c_VIZ_PVR_fname_cmap_cbar_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_PVR_fname_cmap_cbar_ctl')
!!      type(c_ptr) function c_VIZ_PVR_cmap_cbar_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_PVR_cmap_cbar_ctl')
!!      type(c_ptr) function c_VIZ_PVR_movie_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_movie_ctl')
!!      type(c_ptr) function c_VIZ_PVR_quilt_image_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_PVR_quilt_image_ctl')
!!      type(c_ptr) function c_VIZ_PVR_updated_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_PVR_updated_ctl')
!!      type(c_ptr) function c_VIZ_PVR_file_head_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_PVR_file_head_ctl')
!!      type(c_ptr) function c_VIZ_PVR_file_fmt_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_PVR_file_fmt_ctl')
!!      type(c_ptr) function c_VIZ_PVR_monitoring_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_PVR_monitoring_ctl')
!!      type(c_ptr) function c_VIZ_PVR_streo_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_streo_ctl')
!!      type(c_ptr) function c_VIZ_PVR_anaglyph_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_PVR_anaglyph_ctl')
!!      type(c_ptr) function c_VIZ_PVR_quilt_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_quilt_ctl')
!!      type(c_ptr) function c_VIZ_PVR_render_area_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_PVR_render_area_ctl')
!!      type(c_ptr) function c_VIZ_PVR_field_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PVR_field_ctl')
!!      type(c_ptr) function c_VIZ_PVR_component_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_PVR_component_ctl')
!!      type(c_ptr) function c_VIZ_PVR_sections_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_PVR_sections_ctl')
!!      type(c_ptr) function c_VIZ_PVR_isosurfaces_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_PVR_isosurfaces_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_PVR_ctl
!
      use iso_c_binding
      use t_control_data_4_pvr
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_PVR_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_PVR_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_ctl_iflag = C_loc(f_ctl%i_pvr_ctl)
      end function c_VIZ_PVR_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_fname_mat_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_PVR_fname_mat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_fname_mat_ctl = C_loc(f_ctl%fname_mat_ctl)
      end function c_VIZ_PVR_fname_mat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_viewmat_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_PVR_viewmat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_viewmat_ctl = C_loc(f_ctl%mat)
      end function c_VIZ_PVR_viewmat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_fname_pvr_light_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_PVR_fname_pvr_light_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_fname_pvr_light_ctl = C_loc(f_ctl%fname_pvr_light_c)
      end function c_VIZ_PVR_fname_pvr_light_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_light_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_light_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_light_ctl = C_loc(f_ctl%light)
      end function c_VIZ_PVR_light_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_fname_cmap_cbar_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_PVR_fname_cmap_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_fname_cmap_cbar_ctl = C_loc(f_ctl%fname_cmap_cbar_c)
      end function c_VIZ_PVR_fname_cmap_cbar_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_cmap_cbar_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_PVR_cmap_cbar_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_cmap_cbar_ctl = C_loc(f_ctl%cmap_cbar_c)
      end function c_VIZ_PVR_cmap_cbar_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_movie_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_movie_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_movie_ctl = C_loc(f_ctl%movie)
      end function c_VIZ_PVR_movie_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_quilt_image_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_PVR_quilt_image_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_quilt_image_ctl = C_loc(f_ctl%quilt_c)
      end function c_VIZ_PVR_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_updated_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_PVR_updated_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_updated_ctl = C_loc(f_ctl%updated_ctl)
      end function c_VIZ_PVR_updated_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_file_head_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_PVR_file_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_file_head_ctl = C_loc(f_ctl%file_head_ctl)
      end function c_VIZ_PVR_file_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_file_fmt_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_PVR_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_file_fmt_ctl = C_loc(f_ctl%file_fmt_ctl)
      end function c_VIZ_PVR_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_monitoring_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_PVR_monitoring_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_monitoring_ctl = C_loc(f_ctl%monitoring_ctl)
      end function c_VIZ_PVR_monitoring_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_streo_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_streo_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_streo_ctl = C_loc(f_ctl%streo_ctl)
      end function c_VIZ_PVR_streo_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_anaglyph_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_PVR_anaglyph_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_anaglyph_ctl = C_loc(f_ctl%anaglyph_ctl)
      end function c_VIZ_PVR_anaglyph_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_quilt_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_quilt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_quilt_ctl = C_loc(f_ctl%quilt_ctl)
      end function c_VIZ_PVR_quilt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_render_area_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_PVR_render_area_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_render_area_ctl = C_loc(f_ctl%render_area_c)
      end function c_VIZ_PVR_render_area_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_field_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PVR_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_field_ctl = C_loc(f_ctl%pvr_field_ctl)
      end function c_VIZ_PVR_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_component_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_PVR_component_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_component_ctl = C_loc(f_ctl%pvr_comp_ctl)
      end function c_VIZ_PVR_component_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_sections_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_PVR_sections_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_sections_ctl = C_loc(f_ctl%pvr_scts_c)
      end function c_VIZ_PVR_sections_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PVR_isosurfaces_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_PVR_isosurfaces_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PVR_isosurfaces_ctl = C_loc(f_ctl%pvr_isos_c)
      end function c_VIZ_PVR_isosurfaces_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_PVR_ctl
