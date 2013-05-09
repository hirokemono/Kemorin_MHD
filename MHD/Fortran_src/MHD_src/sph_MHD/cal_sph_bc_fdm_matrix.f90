!> @file cal_sph_bc_fdm_matrix.f90
!!      module cal_sph_bc_fdm_matrix
!!
!! @author H. Matsui
!! @date Written on May, 2003
!
!!> @brief calculate FDM matrices for boundaries
!!
!!@verbatim
!!      subroutine s_cal_sph_bc_fdm_matrices
!!@endverbatim
!
      module cal_sph_bc_fdm_matrix
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
      subroutine s_cal_sph_bc_fdm_matrices
!
      use m_spheric_parameter
      use m_coef_fdm_fixed_ICB
      use m_coef_fdm_fixed_CMB
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
      use m_vp_coef_fdm4_nonslip_ICB
      use m_vp_coef_fdm4_nonslip_CMB
      use m_vp_coef_fdm4_free_ICB
      use m_vp_coef_fdm4_free_CMB
!      use cal_sph_bc_4th_fdm_matrices
!
!
      call cal_2nd_nod_ICB_fixed_fdm(radius_1d_rj_r(nlayer_ICB))
      call cal_2nd_nod_ICB_fix_df_fdm(radius_1d_rj_r(nlayer_ICB))
!
      call cal_2nd_nod_CMB_fixed_fdm(radius_1d_rj_r(nlayer_CMB-2))
      call cal_2nd_nod_CMB_fix_df_fdm(radius_1d_rj_r(nlayer_CMB-1))
!
!
      call cal_2nd_ICB_free_vp_bc_fdm(radius_1d_rj_r(nlayer_ICB))
      call cal_2nd_ICB_free_vt_bc_fdm(radius_1d_rj_r(nlayer_ICB))
!
      call cal_2nd_CMB_free_vp_bc_fdm(radius_1d_rj_r(nlayer_CMB-1))
      call cal_2nd_CMB_free_vt_bc_fdm(radius_1d_rj_r(nlayer_CMB-1))
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
        call check_coef_fdm_fix_dr_ICB
        call check_coef_fdm_fix_dr_CMB
        call check_coef_fdm_free_ICB
        call check_coef_fdm_free_CMB
!
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
      end subroutine s_cal_sph_bc_fdm_matrices
!
! -----------------------------------------------------------------------
!
      end module cal_sph_bc_fdm_matrix
