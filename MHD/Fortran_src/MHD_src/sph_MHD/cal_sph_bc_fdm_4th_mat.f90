!> @file cal_sph_bc_fdm_4th_mat.f90
!!      module cal_sph_bc_fdm_4th_mat
!!
!! @author H. Matsui
!! @date Written on May, 2003
!
!!> @brief calculate 4th order FDM matrices for boundaries
!!
!!@verbatim
!!      subroutine s_cal_sph_bc_fdm_4th_mat                             &
!!     &         (nri, radius_1d_rj_r, sph_bc_U)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!@endverbatim
!
      module cal_sph_bc_fdm_4th_mat
!
      use m_precision
      use m_machine_parameter
!
      use t_coef_fdm4_MHD_boundaries
!
      implicit none
!
      type(fdm4_ICB_vpol), save :: fdm4_noslip_ICB1
      type(fdm4_ICB_vpol), save :: fdm4_free_ICB1
!
      type(fdm4_CMB_vpol), save :: fdm4_noslip_CMB1
      type(fdm4_CMB_vpol), save :: fdm4_free_CMB1
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sph_bc_fdm_4th_mat                               &
     &         (nri, radius_1d_rj_r, sph_bc_U)
!
      use t_boundary_params_sph_MHD
      use coef_fdm4_vpol_nonslip_ICB
      use coef_fdm4_vpol_nonslip_CMB
      use coef_fdm4_vpol_free_ICB
      use coef_fdm4_vpol_free_CMB
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
!
      call cal_fdm4_ICB0_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), fdm4_noslip_ICB1)
      call cal_fdm4_ICB1_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), fdm4_noslip_ICB1)
!
      call cal_fdm4_CMB0_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_out-3), fdm4_noslip_CMB1)
      call cal_fdm4_CMB1_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_out-3), fdm4_noslip_CMB1)
!
      call cal_fdm4_ICB0_free_vp                                        &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), fdm4_free_ICB1)
      call cal_fdm4_ICB1_free_vp                                        &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), fdm4_free_ICB1)
!
      call cal_fdm4_CMB0_free_vp                                        &
     &   (radius_1d_rj_r(sph_bc_U%kr_out-3), fdm4_free_CMB1)
      call cal_fdm4_CMB1_free_vp                                        &
     &   (radius_1d_rj_r(sph_bc_U%kr_out-3), fdm4_free_CMB1)
!
      if (iflag_debug .eq. iflag_full_msg) then
        call check_4th_ICB_nonslip_vp_fdm(fdm4_noslip_ICB1)
        call check_4th_CMB_nonslip_vp_fdm(fdm4_noslip_CMB1)
        call check_4th_ICB_free_vp_fdm(fdm4_free_ICB1)
        call check_4th_CMB_free_vp_fdm(fdm4_free_CMB1)
      end if
!
!      call cal_sph_bc_2nd_ele_fdm_mat
!      call s_cal_sph_bc_4th_fdm_matrices
!
      end subroutine s_cal_sph_bc_fdm_4th_mat
!
! -----------------------------------------------------------------------
!
      end module cal_sph_bc_fdm_4th_mat
