!>@file   set_sph_pol_viscous_ICB.f90
!!@brief  module set_sph_pol_viscous_ICB
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set poloidal diffusivity at ICB
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine add_exp2_sph_pol_viscous_ICB(k_ICB, nnod_rj, jmax,   &
!!     &          mat1_grad_p_ICB, mat2_viscous_ICB, d_vpol, press_e,   &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_ICB(1:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous_ICB(jmax,0:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_pol_viscous_ICB1(k_ICB, nnod_rj, jmax,  &
!!     &          mat3_grad_p_ICB1, mat4_viscous_ICB1, d_vpol, press_e, &
!!     &          d_viscous_p)
!!      subroutine add_exp4_sph_pol_viscous_ICB(k_ICB, nnod_rj, jmax,   &
!!     &          mat3_grad_p_ICB, mat4_viscous_ICB, d_vpol, press_e,   &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_ICB1(0:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_ICB1(jmax,-1:2)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_ICB(1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_ICB(jmax,0:2)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!@endverbatim
!
      module set_sph_pol_viscous_ICB
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
      subroutine add_exp2_sph_pol_viscous_ICB(k_ICB, nnod_rj, jmax,     &
     &          mat1_grad_p_ICB, mat2_viscous_ICB, d_vpol, press_e,     &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p_ICB(1:1)
      real(kind = kreal), intent(in) :: mat2_viscous_ICB(jmax,0:1)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_p1)
      do j = 1, jmax
        inod = j + (k_ICB-1) * jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                        + mat2_viscous_ICB(j, 0) * d_vpol(inod)   &
     &                        - mat1_grad_p_ICB( 1) *  press_e(i_p1)    &
     &                        + mat2_viscous_ICB(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_exp2_sph_pol_viscous_ICB
!
!  -------------------------------------------------------------------
!
      subroutine add_exp4_sph_pol_viscous_ICB1(k_ICB, nnod_rj, jmax,    &
     &          mat3_grad_p_ICB1, mat4_viscous_ICB1, d_vpol, press_e,   &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_ICB1(0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_ICB1(jmax,-1:2)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,inod,i_n1,i_p1,i_p2)
      do j = 1, jmax
        inod = j + k_ICB * jmax
        i_n1 = inod - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                        + mat4_viscous_ICB1(j,-1) * d_vpol(i_n1)  &
     &                        - mat3_grad_p_ICB1( 0) *  press_e(inod)   &
     &                        + mat4_viscous_ICB1(j, 0) * d_vpol(inod)  &
     &                        - mat3_grad_p_ICB1( 1) *  press_e(i_p1)   &
     &                        + mat4_viscous_ICB1(j, 1) * d_vpol(i_p1)  &
     &                        - mat3_grad_p_ICB1( 2) *  press_e(i_p2)   &
     &                        + mat4_viscous_ICB1(j, 2) * d_vpol(i_p2)
      end do
!$omp end parallel do
!
      end subroutine add_exp4_sph_pol_viscous_ICB1
!
!  -------------------------------------------------------------------
!
      subroutine add_exp4_sph_pol_viscous_ICB(k_ICB, nnod_rj, jmax,     &
     &          mat3_grad_p_ICB, mat4_viscous_ICB, d_vpol, press_e,     &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_ICB(1:2)
      real(kind = kreal), intent(in) :: mat4_viscous_ICB(jmax,0:2)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p2, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_p1,i_p2)
      do j = 1, jmax
        inod = j + (k_ICB-1) * jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                        + mat4_viscous_ICB(j, 0) * d_vpol(inod)   &
     &                        - mat3_grad_p_ICB( 1) *  press_e(i_p1)    &
     &                        + mat4_viscous_ICB(j, 1) * d_vpol(i_p1)   &
     &                        - mat3_grad_p_ICB( 2) *  press_e(i_p2)    &
     &                        + mat4_viscous_ICB(j, 2) * d_vpol(i_p2)
      end do
!$omp end parallel do
!
      end subroutine add_exp4_sph_pol_viscous_ICB
!
! -----------------------------------------------------------------------
!
      end module set_sph_pol_viscous_ICB

