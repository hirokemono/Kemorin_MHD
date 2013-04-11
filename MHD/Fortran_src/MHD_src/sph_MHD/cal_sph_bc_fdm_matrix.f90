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
      use m_coef_fdm_fixed_ICB
      use m_coef_fdm_fixed_CMB
      use m_coef_fdm_free_ICB
      use m_coef_fdm_free_CMB
!      use cal_sph_bc_4th_fdm_matrices
!
      call cal_2nd_nod_ICB_fixed_fdm
      call cal_2nd_nod_ICB_fix_df_fdm
      call set_fixed_icb_fdm_mat_coefs
!
      call cal_2nd_nod_CMB_fixed_fdm
      call cal_2nd_nod_CMB_fix_df_fdm
      call set_fixed_cmb_fdm_mat_coefs
!
!
      call cal_2nd_nod_ICB_free_bc_fdm
      call set_free_icb_fdm_mat_coefs
!
      call cal_2nd_nod_CMB_free_bc_fdm
      call set_free_cmb_fdm_mat_coefs
!
      if (iflag_debug .eq. iflag_full_msg) then
        call check_coef_fdm_fix_dr_ICB
        call check_coef_fdm_fix_dr_CMB
        call check_coef_fdm_free_ICB
        call check_coef_fdm_free_CMB
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
