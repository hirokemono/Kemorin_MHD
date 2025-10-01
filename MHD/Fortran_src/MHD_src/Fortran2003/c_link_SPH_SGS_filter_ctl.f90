!>@file   c_link_SPH_SGS_filter_ctl.f90
!!@brief  module c_link_SPH_SGS_filter_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_SGS_sph_filter_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_block_name')
!!      type(c_ptr) function c_SGS_sph_filter_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_iflag')
!!
!!      type(c_ptr) function c_SPH_SGS_sph_filter_type_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_type_ctl')
!!      type(c_ptr) function c_SPH_SGS_r_filter_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SPH_SGS_r_filter_type_ctl')
!!      type(c_ptr) function c_SPH_SGS_maximum_moments_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SPH_SGS_maximum_moments_ctl')
!!      type(c_ptr) function c_SPH_SGS_sph_filter_width_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_width_ctl')
!!      type(c_ptr) function c_SPH_SGS_r_filter_width_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_SPH_SGS_r_filter_width_ctl')
!!      type(c_ptr) function c_SPH_SGS_first_reference_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SPH_SGS_first_reference_ctl')
!!      type(c_ptr) function c_SPH_SGS_second_reference_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SPH_SGS_second_reference_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_SPH_SGS_filter_ctl
!
      use iso_c_binding
      use t_ctl_data_SGS_filter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_sph_filter_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_sph_filter_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_sph_filter_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_sph_filter_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_sph_filter_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_sph_filter_ctl_iflag = C_loc(f_ctl%i_sph_filter_ctl)
      end function c_SGS_sph_filter_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_sph_filter_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_sph_filter_type_ctl = C_loc(f_ctl%sph_filter_type_ctl)
      end function c_SPH_SGS_sph_filter_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_r_filter_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SPH_SGS_r_filter_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_r_filter_type_ctl = C_loc(f_ctl%radial_filter_type_ctl)
      end function c_SPH_SGS_r_filter_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_maximum_moments_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_maximum_moments_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_maximum_moments_ctl = C_loc(f_ctl%maximum_moments_ctl)
      end function c_SPH_SGS_maximum_moments_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_sph_filter_width_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SPH_SGS_sph_filter_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_sph_filter_width_ctl                                    &
     &            = C_loc(f_ctl%sphere_filter_width_ctl)
      end function c_SPH_SGS_sph_filter_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_r_filter_width_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SPH_SGS_r_filter_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_r_filter_width_ctl                                      &
     &            = C_loc(f_ctl%radial_filter_width_ctl)
      end function c_SPH_SGS_r_filter_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_first_reference_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SPH_SGS_first_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_first_reference_ctl = C_loc(f_ctl%first_reference_ctl)
      end function c_SPH_SGS_first_reference_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SPH_SGS_second_reference_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SPH_SGS_second_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_filter_ctl_type), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SPH_SGS_second_reference_ctl= C_loc(f_ctl%second_reference_ctl)
      end function c_SPH_SGS_second_reference_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_SPH_SGS_filter_ctl
