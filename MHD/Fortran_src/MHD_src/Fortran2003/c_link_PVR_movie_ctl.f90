!>@file   c_link_PVR_movie_ctl.f90
!!@brief  module c_link_PVR_movie_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_PVR_movie_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_movie_ctl_block_name')
!!      type(c_ptr) function c_PVR_movie_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_PVR_movie_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_PVR_movie_mode_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_PVR_movie_mode_ctl')
!!      type(c_ptr) function c_PVR_movie_num_frames_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_movie_num_frames_ctl')
!!      type(c_ptr) function c_PVR_movie_rotation_axis_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_movie_rotation_axis_ctl')
!!      type(c_ptr) function c_PVR_movie_angle_range_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_PVR_movie_angle_range_ctl')
!!      type(c_ptr) function c_PVR_movie_apature_range_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_movie_apature_range_ctl')
!!      type(c_ptr) function c_PVR_movie_LIC_kernel_rge_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_movie_LIC_kernel_rge_ctl')
!!      type(c_ptr) function c_PVR_movie_fname_view_st_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_PVR_movie_fname_view_st_ctl')
!!      type(c_ptr) function c_PVR_movie_fname_view_end_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_PVR_movie_fname_view_end_ctl')
!!      type(c_ptr) function c_PVR_movie_view_start_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_PVR_movie_view_start_ctl')
!!      type(c_ptr) function c_PVR_movie_view_end_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_PVR_movie_view_end_ctl')
!!      type(c_ptr) function c_PVR_movie_mul_mmats_c(c_ctl)             &
!!     &          bind(C, NAME = 'c_PVR_movie_mul_mmats_c')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_PVR_movie_ctl
!
      use iso_c_binding
      use t_ctl_data_pvr_movie
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_movie_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_PVR_movie_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_PVR_movie_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_ctl_iflag = C_loc(f_ctl%i_pvr_rotation)
      end function c_PVR_movie_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_mode_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_PVR_movie_mode_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_mode_ctl = C_loc(f_ctl%movie_mode_ctl)
      end function c_PVR_movie_mode_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_num_frames_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_movie_num_frames_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_num_frames_ctl = C_loc(f_ctl%num_frames_ctl)
      end function c_PVR_movie_num_frames_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_rotation_axis_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_movie_rotation_axis_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_rotation_axis_ctl = C_loc(f_ctl%rotation_axis_ctl)
      end function c_PVR_movie_rotation_axis_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_angle_range_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_PVR_movie_angle_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_angle_range_ctl = C_loc(f_ctl%angle_range_ctl)
      end function c_PVR_movie_angle_range_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_apature_range_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_movie_apature_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_apature_range_ctl = C_loc(f_ctl%apature_range_ctl)
      end function c_PVR_movie_apature_range_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_LIC_kernel_rge_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_movie_LIC_kernel_rge_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_LIC_kernel_rge_ctl                                    &
     &            = C_loc(f_ctl%LIC_kernel_peak_range_ctl)
      end function c_PVR_movie_LIC_kernel_rge_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_fname_view_st_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_PVR_movie_fname_view_st_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_fname_view_st_ctl = C_loc(f_ctl%fname_view_start_ctl)
      end function c_PVR_movie_fname_view_st_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_fname_view_end_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_PVR_movie_fname_view_end_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_fname_view_end_ctl = C_loc(f_ctl%fname_view_end_ctl)
      end function c_PVR_movie_fname_view_end_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_view_start_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_PVR_movie_view_start_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_view_start_ctl = C_loc(f_ctl%view_start_ctl)
      end function c_PVR_movie_view_start_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_view_end_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_PVR_movie_view_end_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_view_end_ctl = C_loc(f_ctl%view_end_ctl)
      end function c_PVR_movie_view_end_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_PVR_movie_mul_mmats_c(c_ctl)               &
     &          bind(C, NAME = 'c_PVR_movie_mul_mmats_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pvr_movie_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_PVR_movie_mul_mmats_c = C_loc(f_ctl%mul_mmats_c)
      end function c_PVR_movie_mul_mmats_c
!
!  ---------------------------------------------------------------------
!
      end module c_link_PVR_movie_ctl
