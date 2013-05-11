!>@file   set_radius_func.f90
!!@brief  module set_radius_func
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine set_radius_rot_reft_dat_4_sph(r_hot, r_cold,         &
!!     &          temp_hot, temp_cold, rotate)
!!      subroutine cal_fdm_matrices
!!      subroutine cal_fdm_coeffients
!!@endverbatim
!!
!!@n @param r_hot        radius at highest temperature point
!!@n @param r_cold       radius at lowest temperature point
!!@n @param temp_hot     temperature at highest temperature point
!!@n @param temp_cold    temperature at lowest temperature point
!!@n @param rotate(3)    rotation vector
!
      module set_radius_func
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
      private :: const_2nd_fdm_matrices, const_2nd_fdm_coefs
      private :: const_4th_fdm_matrices, const_4th_fdm_coefs
      private :: const_2e_fdm_coefs
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_radius_rot_reft_dat_4_sph(r_hot, r_cold,           &
     &          temp_hot, temp_cold, rotate)
!
      use m_sph_spectr_data
      use m_sph_phys_address
      use set_radius_func_noequi
!      use set_radius_func_cheby
!
      use set_radius_4_sph_dynamo
      use set_reference_temp_sph
      use set_poloidal_rotation
!
      real(kind = kreal), intent(in) :: r_hot, r_cold
      real(kind = kreal), intent(in) :: temp_hot, temp_cold
      real(kind = kreal), intent(in) :: rotate(3)
!
      
!
!* --------  radius  --------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo
!
!   Choose radial grid mode
      call set_dr_for_nonequi
!      call set_dr_for_cheby
!
      if(iflag_debug .eq. iflag_full_msg) call check_radial_fung_rj
!
!*  ----------   reference of temperature --------
!*
      if (ipol%i_ref_t .gt. 0) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &               write(*,*) 'set_reftemp_4_sph'
        call set_reftemp_4_sph(r_hot, r_cold, temp_hot, temp_cold)
      end if
!*
!*  ----------  rotation of earth  ---------------
!*
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(rotate)
!
      end subroutine set_radius_rot_reft_dat_4_sph
!
!  -------------------------------------------------------------------
!
      subroutine cal_fdm_matrices
!
!
      call const_2nd_fdm_matrices
!      call const_4th_fdm_matrices
!
      end subroutine cal_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm_coeffients
!
!
      call const_2nd_fdm_coefs
!
!      call const_4th_fdm_coefs
!      call const_2e_fdm_coefs
!
      end subroutine cal_fdm_coeffients
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_2nd_fdm_matrices
!
      use m_fdm_coefs
      use set_radius_func_noequi
!      use set_radius_func_cheby
!
!
      call allocate_fdm_matrices(nidx_rj(1))
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi
!      call nod_r_2nd_fdm_coefs_cheby
!
      end subroutine const_2nd_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine const_4th_fdm_matrices
!
      use m_fdm_4th_coefs
      use const_radial_4th_fdm_noequi
!      use const_radial_4th_fdm_cheby
!
!
      call allocate_fdm4_matrices(nidx_rj(1))
!   Choose radial differences
      call nod_r_4th_fdm_coefs_nonequi
!      call nod_r_4th_fdm_coefs_cheby
!
      end subroutine const_4th_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_2nd_fdm_coefs
!
      use m_fdm_coefs
      use set_radius_func_noequi
!      use set_radius_func_cheby
!
!
      call allocate_fdm_coefs(nidx_rj(1))
      call copy_fdm_nod_coefs_from_mat(nidx_rj(1))
      call deallocate_fdm_matrices
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_2_coefs(nidx_rj(1), radius_1d_rj_r(1))
      end if
!
      end subroutine const_2nd_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_4th_fdm_coefs
!
      use m_fdm_4th_coefs
      use const_radial_4th_fdm_noequi
!      use const_radial_4th_fdm_cheby
!
!
      call allocate_fdm4_coefs(nidx_rj(1))
      call copy_fdm4_nod_coefs_from_mat(nidx_rj(1))
      call deallocate_fdm4_matrices
!
if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_4_coefs(nidx_rj(1), radius_1d_rj_r(1))
      end if
!
      end subroutine const_4th_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine const_2e_fdm_coefs
!
      use m_fdm_2e_coefs
!
!
      call allocate_fdm_2e_coefs(nidx_rj(1))
      call cal_2nd_ele_r_fdm_coefs(nlayer_ICB,                          &
     &    nidx_rj(1), radius_1d_rj_r)
!
      end subroutine const_2e_fdm_coefs
!
! -----------------------------------------------------------------------
!
      end module set_radius_func
