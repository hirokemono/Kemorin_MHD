!>@file   PVR_control_label_to_c.f90
!!@brief  module PVR_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for volume rendering
!!
!!@verbatim
!!      type(C_ptr) function c_pvr_movie_mode_list(c_ctl)               &
!!     &          bind(C, NAME = 'c_pvr_movie_mode_list')
!!      type(C_ptr) function c_lic_movie_mode_list(c_ctl)               &
!!     &          bind(C, NAME = 'c_lic_movie_mode_list')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(C_ptr) function c_link_fline_start_list(c_ctl)             &
!!     &          bind(C, NAME = 'c_link_fline_start_list')
!!      type(C_ptr) function c_link_fline_dir_list(c_ctl)               &
!!     &          bind(C, NAME = 'c_link_fline_dir_list')
!!      type(C_ptr) function c_link_fline_seed_list(c_ctl)              &
!!     &          bind(C, NAME = 'c_link_fline_seed_list')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
!
      module PVR_control_label_to_c
!
      use ISO_C_BINDING
!
      use m_precision
      use t_control_array_character
!
      implicit  none
!
      type(ctl_array_chara), save, private, target :: pvr_mve_list
      type(ctl_array_chara), save, private, target :: lic_mve_list
!
      type(ctl_array_chara), save, private, target :: flne_start_list
      type(ctl_array_chara), save, private, target :: flne_dir_list
      type(ctl_array_chara), save, private, target :: flne_seed_list
!
! -----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_pvr_movie_mode_list(c_ctl)                 &
     &          bind(C, NAME = 'c_pvr_movie_mode_list')
      use m_pvr_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(pvr_mve_list%c_tbl))                           &
     &      call pvr_movie_mode_list_array(pvr_mve_list)
      c_pvr_movie_mode_list = C_loc(pvr_mve_list)
      end function c_pvr_movie_mode_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_lic_movie_mode_list(c_ctl)                 &
     &          bind(C, NAME = 'c_lic_movie_mode_list')
      use m_pvr_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(lic_mve_list%c_tbl))                           &
     &      call lic_movie_mode_list_array(lic_mve_list)
      c_lic_movie_mode_list = C_loc(lic_mve_list)
      end function c_lic_movie_mode_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_fline_start_list(c_ctl)               &
     &          bind(C, NAME = 'c_link_fline_start_list')
      use m_control_fline_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(flne_start_list%c_tbl))                        &
     &      call fline_start_label_array(flne_start_list)
      c_link_fline_start_list = C_loc(flne_start_list)
      end function c_link_fline_start_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_fline_dir_list(c_ctl)                 &
     &          bind(C, NAME = 'c_link_fline_dir_list')
      use m_control_fline_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(flne_dir_list%c_tbl))                          &
     &      call fline_direction_label_array(flne_dir_list)
      c_link_fline_dir_list = C_loc(flne_dir_list)
      end function c_link_fline_dir_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_fline_seed_list(c_ctl)                &
     &          bind(C, NAME = 'c_link_fline_seed_list')
      use m_control_fline_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(flne_seed_list%c_tbl))                         &
     &      call fline_seeds_label_array(flne_seed_list)
      c_link_fline_seed_list = C_loc(flne_seed_list)
      end function c_link_fline_seed_list
!
! ----------------------------------------------------------------------
!
      end module PVR_control_label_to_c
