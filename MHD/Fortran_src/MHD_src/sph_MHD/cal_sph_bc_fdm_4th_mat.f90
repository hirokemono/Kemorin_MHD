!> @file cal_sph_bc_fdm_4th_mat.f90
!!      module cal_sph_bc_fdm_4th_mat
!!
!! @author H. Matsui
!! @date Written on May, 2003
!
!!> @brief calculate 4th order FDM matrices for boundaries
!!
!!@verbatim
!!      subroutine s_cal_sph_bc_fdm_4th_mat(nri, h_rho, radius_1d_rj_r, &
!!     &          sph_bc_U, fdm_4th, sph_MHD_bc)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: h_rho
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(fdm_matrices), intent(in) :: fdm_4th
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!@endverbatim
!
      module cal_sph_bc_fdm_4th_mat
!
      use m_precision
      use m_machine_parameter
!
      use t_boundary_data_sph_MHD
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sph_bc_fdm_4th_mat(nri, h_rho, radius_1d_rj_r,   &
     &          sph_bc_U, fdm_4th, sph_MHD_bc)
!
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_coef_fdm4_zero_vpol_CMB
      use t_coef_fdm4_free_vpol_CMB
      use t_coef_fdm4_zero_vpol_ICB
      use t_coef_fdm4_free_vpol_ICB
      use t_coef_fdm4_vpol_centre
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: h_rho
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm_matrices), intent(in) :: fdm_4th
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      call cal_fdm4_ICB0_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), sph_MHD_bc%fdm4_noslip_ICB)
      call cal_fdm4_ICB1_nonslip_vp                                     &
     &   (radius_1d_rj_r(sph_bc_U%kr_in), sph_MHD_bc%fdm4_noslip_ICB)
!
      call cal_fdm4_CMB0_nonslip_vp                                     &
     &  (radius_1d_rj_r(sph_bc_U%kr_out-3), sph_MHD_bc%fdm4_noslip_CMB)
      call cal_fdm4_CMB1_nonslip_vp                                     &
     &  (radius_1d_rj_r(sph_bc_U%kr_out-3), sph_MHD_bc%fdm4_noslip_CMB)
!
      call cal_fdm4_ICB0_free_vp(h_rho, radius_1d_rj_r(sph_bc_U%kr_in), &
     &                           sph_MHD_bc%fdm4_free_ICB)
      call cal_fdm4_ICB1_free_vp(radius_1d_rj_r(sph_bc_U%kr_in),        &
     &                           sph_MHD_bc%fdm4_free_ICB)
!
      call cal_fdm4_CMB0_free_vp(radius_1d_rj_r(sph_bc_U%kr_out-3),     &
     &                           sph_MHD_bc%fdm4_free_vp_CMB)
      call cal_fdm4_CMB1_free_vp(radius_1d_rj_r(sph_bc_U%kr_out-3),     &
     &                           sph_MHD_bc%fdm4_free_vp_CMB)
!
      call cal_coef_fdm4_vpol_centre(radius_1d_rj_r(1), fdm_4th%fdm,    &
     &                               sph_MHD_bc%fdm4_center)
!
      if (iflag_debug .eq. iflag_full_msg) then
        call check_4th_ICB_nonslip_vp_fdm(sph_MHD_bc%fdm4_noslip_ICB)
        call check_4th_CMB_nonslip_vp_fdm(sph_MHD_bc%fdm4_noslip_CMB)
        call check_4th_ICB_free_vp_fdm(sph_MHD_bc%fdm4_free_ICB)
        call check_4th_CMB_free_vp_fdm(sph_MHD_bc%fdm4_free_vp_CMB)
        call check_4th_CMB_free_vp_fdm(sph_MHD_bc%fdm4_free_vp_CMB)
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
