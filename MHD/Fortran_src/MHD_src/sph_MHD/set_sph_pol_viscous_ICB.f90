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
!!
!!      subroutine sub_sph_pol_viscous_mat9_ICB1(k_ICB, nri, jmax,      &
!!     &          mat3_grad_p_ICB1, mat4_viscous_CMB1, mat9)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_ICB1(-1:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine set_sph_pol_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!!        integer(kind = kint), intent(in) :: k_ICB, nri, jmax
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
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
! -----------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat7_ICB(k_ICB, nri, jmax, mat7)
!
      integer(kind = kint), intent(in) :: k_ICB, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-3 .gt. 0) mat7(7,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat7(6,2*k_ICB-2,j) = zero
        if(2*k_ICB-1 .gt. 0) mat7(5,2*k_ICB-1,j) = zero
!
        mat7(4,2*k_ICB,  j) = one
!
        mat7(3,2*k_ICB+1,j) = zero
        mat7(2,2*k_ICB+2,j) = zero
        mat7(1,2*k_ICB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat7_ICB
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_ICB1(k_ICB, nri, jmax,        &
     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-1:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-2 .gt. 0)  mat9(9,2*k_ICB-2,j) = zero
        if(2*k_ICB-1 .gt. 0)  mat9(8,2*k_ICB-1,j) = zero
        mat9(7,2*k_ICB,  j) = mat9(7,2*k_ICB,  j)                       &
     &                       - mat4_viscous_CMB1(j,-1)
        mat9(6,2*k_ICB+1,j) = mat9(6,2*k_ICB+1,j)                       &
     &                       + mat3_grad_p_CMB1(0)
!
        mat9(5,2*k_ICB+2,j) = mat9(5,2*k_ICB+2,j)                       &
     &                       - mat4_viscous_CMB1(j, 0)
!
        mat9(4,2*k_ICB+3,j) = mat9(4,2*k_ICB+3,j)                       &
     &                       + mat3_grad_p_CMB1(1)
        mat9(3,2*k_ICB+4,j) = mat9(3,2*k_ICB+4,j)                       &
     &                       - mat4_viscous_CMB1(j, 1)
        mat9(2,2*k_ICB+5,j) = mat9(2,2*k_ICB+5,j)                       &
     &                       + mat3_grad_p_CMB1(2)
        mat9(1,2*k_ICB+6,j) = mat9(1,2*k_ICB+6,j)                       &
     &                       - mat4_viscous_CMB1(j, 2)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat9_ICB1
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_pol_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-4 .gt. 0) mat9(9,2*k_ICB-4,j) = zero
        if(2*k_ICB-3 .gt. 0) mat9(8,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat9(7,2*k_ICB-2,j) = zero
        if(2*k_ICB-1 .gt. 0) mat9(6,2*k_ICB-1,j) = zero
!
        mat9(5,2*k_ICB,  j) = one
!
        mat9(4,2*k_ICB+1,j) = zero
        mat9(3,2*k_ICB+2,j) = zero
        mat9(2,2*k_ICB+3,j) = zero
        mat9(1,2*k_ICB+4,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_pol_viscous_mat9_ICB
!
! -----------------------------------------------------------------------
!
      end module set_sph_pol_viscous_ICB

