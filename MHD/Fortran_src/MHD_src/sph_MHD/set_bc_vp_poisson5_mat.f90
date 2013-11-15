!>@file   set_bc_vp_poisson5_mat.f90
!!@brief  module set_bc_vp_poisson5_mat
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct poisson matrix for poloidal elocity at boundaries
!!
!!@verbatim
!!      subroutine set_rgd_icb_vp_poisson5_mat
!!      subroutine set_rgd_cmb_vp_poisson5_mat
!!      subroutine set_free_icb_vp_poisson5_mat
!!      subroutine set_free_cmb_vp_poisson5_mat
!!@endverbatim
!
      module set_bc_vp_poisson5_mat
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_physical_property
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_icb_vp_poisson5_mat
!
      use m_vp_coef_fdm4_nonslip_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(4,nlayer_ICB,  j)= -fdm4_noslip_ICB1(-1,3)
        vs_poisson_mat(3,nlayer_ICB+1,j) = -fdm4_noslip_ICB1(0,3)       &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(2,nlayer_ICB+2,j)= -fdm4_noslip_ICB1(1,3)
        vs_poisson_mat(1,nlayer_ICB+3,j)= -fdm4_noslip_ICB1(2,3)
!
        vs_poisson_mat(3,nlayer_ICB,  j)= -fdm4_noslip_ICB(0,3)         &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(2,nlayer_ICB+1,j)= -fdm4_noslip_ICB(1,3)
        vs_poisson_mat(1,nlayer_ICB+2,j)= -fdm4_noslip_ICB(2,3)
      end do
!
      end subroutine set_rgd_icb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_rgd_cmb_vp_poisson5_mat
!
      use m_vp_coef_fdm4_nonslip_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(5,nlayer_CMB-3,j)= -fdm4_noslip_CMB1(-2,3)
        vs_poisson_mat(4,nlayer_CMB-2,j)= -fdm4_noslip_CMB1(-1,3)
        vs_poisson_mat(3,nlayer_CMB-1,j)= -fdm4_noslip_CMB1( 0,3)       &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
        vs_poisson_mat(2,nlayer_CMB,  j)= -fdm4_noslip_CMB1( 1,3)
!
        vs_poisson_mat(5,nlayer_CMB-2,j)= -fdm4_noslip_CMB(-2,3)
        vs_poisson_mat(4,nlayer_CMB-1,j)= -fdm4_noslip_CMB(-1,3)
        vs_poisson_mat(3,nlayer_CMB,  j)= -fdm4_noslip_CMB( 0,3)        &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
      end do
!
      end subroutine set_rgd_cmb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_free_icb_vp_poisson5_mat
!
      use m_vp_coef_fdm4_free_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(3,nlayer_ICB,  j)= -coef_fdm_free_ICB_vp4(0,3)   &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(2,nlayer_ICB+1,j)= -coef_fdm_free_ICB_vp4(1,3)
        vs_poisson_mat(1,nlayer_ICB+2,j)= -coef_fdm_free_ICB_vp4(2,3)
!
        vs_poisson_mat(4,nlayer_ICB,  j)= -coef_fdm_free_ICB1_vp4(-1,3)
        vs_poisson_mat(3,nlayer_ICB+1,j)= -coef_fdm_free_ICB1_vp4( 0,3) &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
        vs_poisson_mat(2,nlayer_ICB+2,j)= -coef_fdm_free_ICB1_vp4(1,3)
        vs_poisson_mat(1,nlayer_ICB+3,j)= -coef_fdm_free_ICB1_vp4(2,3)
      end do
!
      end subroutine set_free_icb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_free_cmb_vp_poisson5_mat
!
      use m_vp_coef_fdm4_free_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(5,nlayer_CMB-3,j)= -coef_fdm_free_CMB1_vp4(-2,3)
        vs_poisson_mat(4,nlayer_CMB-2,j)= -coef_fdm_free_CMB1_vp4(-1,3)
        vs_poisson_mat(3,nlayer_CMB-1,j)= -coef_fdm_free_CMB1_vp4( 0,3) &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
        vs_poisson_mat(2,nlayer_CMB,  j)= -coef_fdm_free_CMB1_vp4( 1,3)
!
        vs_poisson_mat(5,nlayer_CMB-2,j)= -coef_fdm_free_CMB_vp4(-2,3)
        vs_poisson_mat(4,nlayer_CMB-1,j)= -coef_fdm_free_CMB_vp4(-1,3)
        vs_poisson_mat(3,nlayer_CMB,  j)= -coef_fdm_free_CMB_vp4( 0,3)  &
     &                    + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
      end do
!
      end subroutine set_free_cmb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
!
      end module set_bc_vp_poisson5_mat
