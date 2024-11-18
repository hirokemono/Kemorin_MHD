!>@file   set_bc_vp_poisson5_mat.f90
!!@brief  module set_bc_vp_poisson5_mat
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct poisson matrix for poloidal elocity at boundaries
!!
!!@verbatim
!!      subroutine rigid_icb_vp_poisson5_mat(nri, jmax, g_sph_rj, kr_in,&
!!     &          r_ICB, r_ICB1, fdm4_noslip_ICB, fdm4_noslip_ICB1,     &
!!     &          poisson_mat5)
!!      subroutine rigid_cmb_vp_poisson5_mat(nri, jmax, g_sph_rj,       &
!!     &          kr_out, r_CMB, r_CMB1, fdm4_noslip_CMB,               &
!!     &          fdm4_noslip_CMB1, poisson_mat5)
!!@endverbatim
!
      module set_bc_vp_poisson5_mat
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine rigid_icb_vp_poisson5_mat(nri, jmax, g_sph_rj, kr_in,  &
     &          r_ICB, r_ICB1, fdm4_noslip_ICB, fdm4_noslip_ICB1,       &
     &          poisson_mat5)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_ICB(0:2), r_ICB1(0:2)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB(0:2,2:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_ICB1(-1:2,5)
!
      real(kind = kreal), intent(inout) :: poisson_mat5(5,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, nri
        poisson_mat5(4,kr_in,  j) = -fdm4_noslip_ICB1(-1,3)
        poisson_mat5(3,kr_in+1,j) = -fdm4_noslip_ICB1(0,3)              &
     &                             + g_sph_rj(j,3)*r_ICB(2)
        poisson_mat5(2,kr_in+2,j) = -fdm4_noslip_ICB1(1,3)
        poisson_mat5(1,kr_in+3,j) = -fdm4_noslip_ICB1(2,3)
!
        poisson_mat5(3,kr_in,  j) = -fdm4_noslip_ICB(0,3)               &
     &                             + g_sph_rj(j,3)*r_ICB1(2)
        poisson_mat5(2,kr_in+1,j) = -fdm4_noslip_ICB(1,3)
        poisson_mat5(1,kr_in+2,j) = -fdm4_noslip_ICB(2,3)
      end do
!
      end subroutine rigid_icb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
!
      subroutine rigid_cmb_vp_poisson5_mat(nri, jmax, g_sph_rj,         &
     &          kr_out, r_CMB, r_CMB1, fdm4_noslip_CMB,                 &
     &          fdm4_noslip_CMB1, poisson_mat5)
!
      integer(kind = kint), intent(in) :: nri, jmax, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,13)
      real(kind = kreal), intent(in) :: r_CMB(0:2), r_CMB1(0:2)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB(-2:0,2:4)
      real(kind = kreal), intent(in) :: fdm4_noslip_CMB1(-2:1,2:5)
!
      real(kind = kreal), intent(inout) :: poisson_mat5(5,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, nri
        poisson_mat5(5,kr_out-3,j) = -fdm4_noslip_CMB1(-2,3)
        poisson_mat5(4,kr_out-2,j) = -fdm4_noslip_CMB1(-1,3)
        poisson_mat5(3,kr_out-1,j) = -fdm4_noslip_CMB1( 0,3)            &
     &                               + g_sph_rj(j,3)*r_CMB1(2)
        poisson_mat5(2,kr_out,  j) = -fdm4_noslip_CMB1( 1,3)
!
        poisson_mat5(5,kr_out-2,j) = -fdm4_noslip_CMB(-2,3)
        poisson_mat5(4,kr_out-1,j) = -fdm4_noslip_CMB(-1,3)
        poisson_mat5(3,kr_out,  j) = -fdm4_noslip_CMB( 0,3)             &
     &                               + g_sph_rj(j,3)*r_CMB(2)
      end do
!
      end subroutine rigid_cmb_vp_poisson5_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module set_bc_vp_poisson5_mat
