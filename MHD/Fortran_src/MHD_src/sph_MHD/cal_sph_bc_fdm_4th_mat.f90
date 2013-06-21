!> @file cal_sph_bc_fdm_4th_mat.f90
!!      module cal_sph_bc_fdm_4th_mat
!!
!! @author H. Matsui
!! @date Written on May, 2003
!
!!> @brief calculate 4th order FDM matrices for boundaries
!!
!!@verbatim
!!      subroutine s_cal_sph_bc_fdm_4th_mat
!!@endverbatim
!
      module cal_sph_bc_fdm_4th_mat
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sph_bc_fdm_4th_mat
!
      use m_spheric_parameter
      use m_vp_coef_fdm4_nonslip_ICB
      use m_vp_coef_fdm4_nonslip_CMB
      use m_vp_coef_fdm4_free_ICB
      use m_vp_coef_fdm4_free_CMB
!      use cal_sph_bc_4th_fdm_matrices
!
!
      call cal_4th_ICB_nonslip_vp_fdm(radius_1d_rj_r(nlayer_ICB))
      call cal_4th_ICB1_nonslip_vp_fdm(radius_1d_rj_r(nlayer_ICB))
!
      call cal_4th_CMB_nonslip_vp_fdm(radius_1d_rj_r(nlayer_CMB-3))
      call cal_4th_CMB1_nonslip_vp_fdm(radius_1d_rj_r(nlayer_CMB-3))
!
      call cal_4th_ICB_free_vp_fdm(radius_1d_rj_r(nlayer_ICB))
      call cal_4th_ICB1_free_vp_fdm(radius_1d_rj_r(nlayer_ICB))
!
      call cal_4th_CMB_free_vp_fdm(radius_1d_rj_r(nlayer_CMB-3))
      call cal_4th_CMB1_free_vp_fdm(radius_1d_rj_r(nlayer_CMB-3))
!
      if (iflag_debug .eq. iflag_full_msg) then
        call check_4th_ICB_nonslip_vp_fdm
        call check_4th_CMB_nonslip_vp_fdm
        call check_4th_ICB_free_vp_fdm
        call check_4th_CMB_free_vp_fdm
      end if
!
!      call cal_sph_bc_2nd_ele_fdm_mat
!      call s_cal_sph_bc_4th_fdm_matrices
!      call cal_sph_center_fdm_matrices
!
      end subroutine s_cal_sph_bc_fdm_4th_mat
!
! -----------------------------------------------------------------------
!
      end module cal_sph_bc_fdm_4th_mat
