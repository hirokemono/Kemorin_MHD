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
      private :: cal_2nd_ele_r_fdm_coefs
      private :: copy_fdm_nod_coefs_from_mat
      private :: copy_fdm_ele_coefs_from_mat
      private :: copy_fdm4_nod_coefs_from_mat
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
      use set_radius_func_cheby
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
      use m_fdm_coefs
!
      use set_radius_func_noequi
      use set_radius_func_cheby
!
!
      call allocate_fdm_matrices(nidx_rj(1))
!
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi
      call nod_r_4th_fdm_coefs_nonequi
!
      call cal_2nd_ele_r_fdm_coefs
!
      end subroutine cal_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm_coeffients
!
      use m_fdm_coefs
!
!
      call allocate_fdm_coefs(nidx_rj(1))
!
      call copy_fdm_nod_coefs_from_mat(nidx_rj(1))
      call copy_fdm_ele_coefs_from_mat(nidx_rj(1))
      call copy_fdm4_nod_coefs_from_mat(nidx_rj(1))
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_2_coefs(nidx_rj(1), radius_1d_rj_r(1))
        call check_fdm_2e_coefs(nidx_rj(1), radius_1d_rj_r(1))
        call check_fdm_4_coefs(nidx_rj(1), radius_1d_rj_r(1))
      end if
!
      call deallocate_fdm_matrices
!
      end subroutine cal_fdm_coeffients
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_2nd_ele_r_fdm_coefs
!
      use m_fdm_coefs
      use cal_inverse_small_matrix
!
      integer(kind = kint) :: ierr
      integer(kind = kint) :: kr
!
      real(kind = kreal) :: dr_p1, dr_n1
      real(kind = kreal) :: mat_taylor_2(2,2)
!
!
      do kr = 1, nidx_rj(1)
!
        dr_p1 = dr_1d_rj(kr,0) * half
        if (kr.eq.1) then
          if(nlayer_ICB.gt.1) then
            dr_n1 = radius_1d_rj_r(1) * half
          else
            dr_n1 = dr_1d_rj(1,0) * half
          end if
        else
          dr_n1 = dr_1d_rj(kr,1) * half
        end if
!
        mat_taylor_2(1,1) = one
        mat_taylor_2(1,2) = dr_p1
!
        mat_taylor_2(2,1) = one
        mat_taylor_2(2,2) =-dr_n1
!
        call cal_inverse_22_matrix(mat_taylor_2, mat_fdm_2e(1,1,kr),    &
     &      ierr)
      end do
!
      end subroutine cal_2nd_ele_r_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_fdm_nod_coefs_from_mat(nri)
!
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: nri
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nri
        d1nod_mat_fdm_2(k,-1) = mat_fdm_2(2,3,k)
        d1nod_mat_fdm_2(k, 0) = mat_fdm_2(2,1,k)
        d1nod_mat_fdm_2(k, 1) = mat_fdm_2(2,2,k)
!
        d2nod_mat_fdm_2(k,-1) = mat_fdm_2(3,3,k)
        d2nod_mat_fdm_2(k, 0) = mat_fdm_2(3,1,k)
        d2nod_mat_fdm_2(k, 1) = mat_fdm_2(3,2,k)
      end do
!$omp end parallel do
!
      end subroutine copy_fdm_nod_coefs_from_mat
!
! -----------------------------------------------------------------------
!
      subroutine copy_fdm_ele_coefs_from_mat(nri)
!
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: nri
      integer(kind = kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nri
        d_nod_mat_fdm_2e(k,0) = mat_fdm_2e(1,2,k)
        d_nod_mat_fdm_2e(k,1) = mat_fdm_2e(1,1,k)
!
        d1nod_mat_fdm_2e(k,0) = mat_fdm_2e(2,2,k)
        d1nod_mat_fdm_2e(k,1) = mat_fdm_2e(2,1,k)
      end do
!$omp end parallel do
!
      end subroutine copy_fdm_ele_coefs_from_mat
!
! -----------------------------------------------------------------------
!
      subroutine copy_fdm4_nod_coefs_from_mat(nri)
!
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: nri
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nri
        d1nod_mat_fdm_4(k,-2) = mat_fdm_4(2,5,k)
        d1nod_mat_fdm_4(k,-1) = mat_fdm_4(2,3,k)
        d1nod_mat_fdm_4(k, 0) = mat_fdm_4(2,1,k)
        d1nod_mat_fdm_4(k, 1) = mat_fdm_4(2,2,k)
        d1nod_mat_fdm_4(k, 2) = mat_fdm_4(2,4,k)
!
        d2nod_mat_fdm_4(k,-2) = mat_fdm_4(3,5,k)
        d2nod_mat_fdm_4(k,-1) = mat_fdm_4(3,3,k)
        d2nod_mat_fdm_4(k, 0) = mat_fdm_4(3,1,k)
        d2nod_mat_fdm_4(k, 1) = mat_fdm_4(3,2,k)
        d2nod_mat_fdm_4(k, 2) = mat_fdm_4(3,4,k)
!
        d3nod_mat_fdm_4(k,-2) = mat_fdm_4(4,5,k)
        d3nod_mat_fdm_4(k,-1) = mat_fdm_4(4,3,k)
        d3nod_mat_fdm_4(k, 0) = mat_fdm_4(4,1,k)
        d3nod_mat_fdm_4(k, 1) = mat_fdm_4(4,2,k)
        d3nod_mat_fdm_4(k, 2) = mat_fdm_4(4,4,k)
!
        d4nod_mat_fdm_4(k,-2) = mat_fdm_4(5,5,k)
        d4nod_mat_fdm_4(k,-1) = mat_fdm_4(5,3,k)
        d4nod_mat_fdm_4(k, 0) = mat_fdm_4(5,1,k)
        d4nod_mat_fdm_4(k, 1) = mat_fdm_4(5,2,k)
        d4nod_mat_fdm_4(k, 2) = mat_fdm_4(5,4,k)
      end do
!$omp end parallel do
!
      end subroutine copy_fdm4_nod_coefs_from_mat
!
! -----------------------------------------------------------------------
!
      end module set_radius_func
