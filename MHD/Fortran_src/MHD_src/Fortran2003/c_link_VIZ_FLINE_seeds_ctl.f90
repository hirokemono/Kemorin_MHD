!>@file   c_link_VIZ_FLINE_seeds_ctl.f90
!!@brief  module c_link_VIZ_FLINE_seeds_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for fieldline seed points controls
!!@verbatim
!!      type(c_ptr) function c_VIZ_FLINE_seeds_ctl_blk_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seeds_ctl_blk_name')
!!      type(c_ptr) function c_VIZ_FLINE_seeds_ctl_iflag(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seeds_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_FLINE_seed_point_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seed_point_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_geological_pnt_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_geological_pnt_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_spherical_pnt_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_spherical_pnt_ctl')
!!
!!      type(c_ptr) function c_VIZ_FLINE_seed_surface_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seed_surface_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_FLINE_seeds_ctl
!
      use iso_c_binding
      use t_fline_seeds_list_ctl
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seeds_ctl_blk_name(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_FLINE_seeds_ctl_blk_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seeds_ctl_blk_name = C_loc(f_ctl%block_name)
      end function c_VIZ_FLINE_seeds_ctl_blk_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seeds_ctl_iflag(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_FLINE_seeds_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seeds_ctl_iflag = C_loc(f_ctl%i_seeds_list_ctl)
      end function c_VIZ_FLINE_seeds_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seed_point_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_FLINE_seed_point_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seed_point_ctl= C_loc(f_ctl%seed_point_ctl)
      end function c_VIZ_FLINE_seed_point_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_geological_pnt_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_FLINE_geological_pnt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_geological_pnt_ctl= C_loc(f_ctl%seed_geological_ctl)
      end function c_VIZ_FLINE_geological_pnt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_spherical_pnt_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_FLINE_spherical_pnt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_spherical_pnt_ctl= C_loc(f_ctl%seed_spherical_ctl)
      end function c_VIZ_FLINE_spherical_pnt_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seed_surface_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_FLINE_seed_surface_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_seeds_list_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seed_surface_ctl= C_loc(f_ctl%seed_surface_ctl)
      end function c_VIZ_FLINE_seed_surface_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_FLINE_seeds_ctl
