!
!      module cal_sph_bc_fdm_matrix
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine s_cal_sph_bc_fdm_matrices
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
      use m_coef_fdm_fixed_ICB
      use m_coef_fdm_fixed_CMB
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
!      use cal_sph_bc_4th_fdm_matrices
!
      call cal_2nd_nod_ICB_fixed_fdm
      call cal_2nd_nod_ICB_fix_df_fdm
      call set_fixed_icb_fdm_mat_coefs
      if (iflag_debug .gt. 0) call check_coef_fdm_fix_dr_ICB
!
      call cal_2nd_nod_CMB_fixed_fdm
      call cal_2nd_nod_CMB_fix_df_fdm
      call set_fixed_cmb_fdm_mat_coefs
      if (iflag_debug .gt. 0) call check_coef_fdm_fix_dr_CMB
!
!
      call cal_2nd_nod_ICB_free_bc_fdm
      call set_free_icb_fdm_mat_coefs
      if (iflag_debug .gt. 0) call check_coef_fdm_free_ICB
!
      call cal_2nd_nod_CMB_free_bc_fdm
      call set_free_cmb_fdm_mat_coefs
      if (iflag_debug .gt. 0) call check_coef_fdm_free_CMB
!
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
