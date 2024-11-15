!>@file   set_sph_pol_viscous_CMB.f90
!!@brief  module set_sph_pol_viscous_CMB
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set horizontal diffusivity at CMB
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine add_exp2_sph_pol_viscous_CMB(k_CMB, nnod_rj, jmax,   &
!!     &          mat1_grad_p_CMB, mat2_viscous_CMB,                    &
!!     &          d_vpol, press_e, d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CMB(0:0)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_pol_viscous_CMB1(k_CMB, nnod_rj, jmax,  &
!!     &          mat3_grad_p_CMB1, mat4_viscous_CMB1,                  &
!!     &          d_vpol, press_e, d_viscous_p)
!!      subroutine add_exp4_sph_pol_viscous_CMB(k_CMB, nnod_rj, jmax,   &
!!     &          mat3_grad_p_CMB1, mat3_grad_p_CMB,                    &
!!     &          mat4_viscous_CMB1, mat4_viscous_CMB,                  &
!!     &          d_vpol, press_e, d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(-1:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB(-1:0)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!@endverbatim
!
      module set_sph_pol_viscous_CMB
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
      subroutine add_exp2_sph_pol_viscous_CMB(k_CMB, nnod_rj, jmax,     &
     &          mat1_grad_p_CMB, mat2_viscous_CMB,                      &
     &          d_vpol, press_e, d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p_CMB(0:0)
      real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, inod
!
!
!$omp parallel do private(j,inod,i_n1)
      do j = 1, jmax
        inod = j + (k_CMB-1) * jmax
        i_n1 = inod - jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                   + mat2_viscous_CMB(j,-1) * d_vpol(i_n1)        &
     &                   - mat1_grad_p_CMB( 0) *  press_e(inod)         &
     &                   + mat2_viscous_CMB(j, 0) * d_vpol(inod)
      end do
!$omp end parallel do
!
      end subroutine add_exp2_sph_pol_viscous_CMB
!
!  -------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module set_sph_pol_viscous_CMB

