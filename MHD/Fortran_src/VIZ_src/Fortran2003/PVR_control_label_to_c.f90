!>@file   PVR_control_label_to_c.f90
!!@brief  module PVR_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for volume rendering
!!
!!@verbatim
!!      type(C_ptr) function c_link_psf_coef_label_list(c_ctl)          &
!!     &          bind(C, NAME = 'c_link_psf_coef_label_list')
!!      type(C_ptr) function c_link_psf_dirs_label_list(c_ctl)          &
!!     &          bind(C, NAME = 'c_link_psf_dirs_label_list')
!!        use m_pvr_control_labels
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(C_ptr) function c_link_isosurf_dir_list(c_ctl)             &
!!     &          bind(C, NAME = 'c_link_isosurf_dir_list')
!!      type(C_ptr) function c_link_surf_enhance_mode_list(c_ctl)       &
!!     &          bind(C, NAME = 'c_link_surf_enhance_mode_list')
!!        use m_pvr_control_labels
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(C_ptr) function c_pvr_movie_mode_list(c_ctl)               &
!!     &          bind(C, NAME = 'c_pvr_movie_mode_list')
!!      type(C_ptr) function c_lic_movie_mode_list(c_ctl)               &
!!     &          bind(C, NAME = 'c_lic_movie_mode_list')
!!        use m_pvr_control_labels
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
      type(ctl_array_chara), save, private, target :: psf_coef_list
      type(ctl_array_chara), save, private, target :: psf_dirs_list
!
      type(ctl_array_chara), save, private, target :: iso_dir_list
      type(ctl_array_chara), save, private, target :: enhance_list
!
      type(ctl_array_chara), save, private, target :: pvr_mve_list
      type(ctl_array_chara), save, private, target :: lic_mve_list
!
! -----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_psf_coef_label_list(c_ctl)            &
     &          bind(C, NAME = 'c_link_psf_coef_label_list')
      use m_section_coef_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(psf_coef_list%c_tbl))                          &
     &      call psf_coef_label_array(psf_coef_list)
      c_link_psf_coef_label_list = C_loc(psf_coef_list)
      end function c_link_psf_coef_label_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_psf_dirs_label_list(c_ctl)            &
     &          bind(C, NAME = 'c_link_psf_dirs_label_list')
      use m_section_coef_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(psf_dirs_list%c_tbl))                          &
     &      call psf_dirs_label_array(psf_dirs_list)
      c_link_psf_dirs_label_list = C_loc(psf_dirs_list)
      end function c_link_psf_dirs_label_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_isosurf_dir_list(c_ctl)               &
     &          bind(C, NAME = 'c_link_isosurf_dir_list')
      use m_pvr_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(iso_dir_list%c_tbl))                           &
     &      call pvr_isosurf_dir_list_array(iso_dir_list)
      c_link_isosurf_dir_list = C_loc(iso_dir_list)
      end function c_link_isosurf_dir_list
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_surf_enhance_mode_list(c_ctl)         &
     &          bind(C, NAME = 'c_link_surf_enhance_mode_list')
      use m_pvr_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(enhance_list%c_tbl))                           &
     &      call pvr_surf_enhance_mode_array(enhance_list)
      c_link_surf_enhance_mode_list = C_loc(enhance_list)
      end function c_link_surf_enhance_mode_list
!
! ----------------------------------------------------------------------
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
!
      end module PVR_control_label_to_c
