!>@file   c_link_VIZ_MAP_ctl.f90
!!@brief  module c_link_VIZ_MAP_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_MAP_ctl_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_MAP_ctl_block_name')
!!      type(c_ptr) function c_VIZ_MAP_ctl_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_MAP_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_MAP_map_define_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_MAP_map_define_ctl')
!!      type(c_ptr) function c_VIZ_MAP_map_image_prefix_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_MAP_map_image_prefix_ctl')
!!      type(c_ptr) function c_VIZ_MAP_map_image_fmt_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_MAP_map_image_fmt_ctl')
!!      type(c_ptr) function c_VIZ_MAP_map_field_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_MAP_map_field_ctl')
!!      type(c_ptr) function c_VIZ_MAP_map_comp_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_MAP_map_comp_ctl')
!!      type(c_ptr) function c_VIZ_MAP_isoline_field_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_MAP_isoline_field_ctl')
!!      type(c_ptr) function c_VIZ_MAP_isoline_comp_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_MAP_isoline_comp_ctl')
!!      type(c_ptr) function c_VIZ_MAP_fname_mat_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_MAP_fname_mat_ctl')
!!      type(c_ptr) function c_VIZ_MAP_viewmat_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_MAP_viewmat_ctl')
!!      type(c_ptr) function c_VIZ_MAP_fname_cmap_cbar_c(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_MAP_fname_cmap_cbar_c')
!!      type(c_ptr) function c_VIZ_MAP_cmap_cbar_c(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_MAP_cmap_cbar_c')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_MAP_ctl
!
      use iso_c_binding
      use t_control_data_4_map
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_MAP_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_MAP_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_MAP_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_ctl_iflag = C_loc(f_ctl%i_map_ctl)
      end function c_VIZ_MAP_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_map_define_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_MAP_map_define_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_map_define_ctl = C_loc(f_ctl%map_define_ctl)
      end function c_VIZ_MAP_map_define_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_map_image_prefix_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_MAP_map_image_prefix_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_map_image_prefix_ctl= C_loc(f_ctl%map_image_prefix_ctl)
      end function c_VIZ_MAP_map_image_prefix_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_map_image_fmt_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_MAP_map_image_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_map_image_fmt_ctl = C_loc(f_ctl%map_image_fmt_ctl)
      end function c_VIZ_MAP_map_image_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_map_field_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_MAP_map_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_map_field_ctl = C_loc(f_ctl%map_field_ctl)
      end function c_VIZ_MAP_map_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_map_comp_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_MAP_map_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_map_comp_ctl = C_loc(f_ctl%map_comp_ctl)
      end function c_VIZ_MAP_map_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_isoline_field_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_MAP_isoline_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_isoline_field_ctl = C_loc(f_ctl%isoline_field_ctl)
      end function c_VIZ_MAP_isoline_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_isoline_comp_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_MAP_isoline_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_isoline_comp_ctl = C_loc(f_ctl%isoline_comp_ctl)
      end function c_VIZ_MAP_isoline_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_fname_mat_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_MAP_fname_mat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_fname_mat_ctl = C_loc(f_ctl%fname_mat_ctl)
      end function c_VIZ_MAP_fname_mat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_viewmat_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_MAP_viewmat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_viewmat_ctl = C_loc(f_ctl%mat)
      end function c_VIZ_MAP_viewmat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_fname_cmap_cbar_c(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_MAP_fname_cmap_cbar_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_fname_cmap_cbar_c = C_loc(f_ctl%fname_cmap_cbar_c)
      end function c_VIZ_MAP_fname_cmap_cbar_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_MAP_cmap_cbar_c(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_MAP_cmap_cbar_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_MAP_cmap_cbar_c = C_loc(f_ctl%cmap_cbar_c)
      end function c_VIZ_MAP_cmap_cbar_c
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_MAP_ctl
