!>@file   set_sph_pol_viscous_CTR.f90
!!@brief  module set_sph_pol_viscous_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set poloidal diffusivity at ICB
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine add_exp2_sph_viscous_CTR1(nnod_rj, jmax,             &
!!     &          d_vpol, press_e, mat1_grad_p_CTR1, mat2_viscous_CTR1, &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_viscous_CTR2(nnod_rj, jmax,             &
!!     &          d_vpol, press_e, mat3_grad_p_CTR1, mat4_viscous_CTR2, &
!!     &          d_viscous_p)
!!         integer(kind = kint), intent(in) :: nnod_rj, jmax
!!         real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(-1:2)
!!         real(kind = kreal), intent(in) :: mat4_viscous_CTR2(jmax,-1:2)
!!         real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!         real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!         real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_viscous_CTR1(nnod_rj, jmax,             &
!!     &          d_vpol, press_e, mat3_grad_p_CTR1, mat4_viscous_CTR1, &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,0:2)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,             &
!!     &          mat1_grad_p_CTR1, mat2_viscous_CTR1, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,nri,jmax)
!!      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,             &
!!     &          mat3_grad_p_CTR1, mat4_viscous_CTR1, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(-1:2)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,-2:2)
!!        real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!!@endverbatim
!
      module set_sph_pol_viscous_CTR
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
      subroutine add_exp2_sph_viscous_CTR1(nnod_rj, jmax,               &
     &          d_vpol, press_e, mat1_grad_p_CTR1, mat2_viscous_CTR1,   &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(0:1)
      real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax,0:1)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,i_p1,inod)
      do j = 1, jmax
        inod = j
!        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
!     &                        + mat2_viscous_CTR1(j,-1) * d_vpol(i_n1) &
     &                        - mat1_grad_p_CTR1( 0) *  press_e(inod)   &
     &                        + mat2_viscous_CTR1(j, 0) * d_vpol(inod)  &
     &                        - mat1_grad_p_CTR1( 1) *  press_e(i_p1)   &
     &                        + mat2_viscous_CTR1(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_exp2_sph_viscous_CTR1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_exp4_sph_viscous_CTR2(nnod_rj, jmax,               &
     &          d_vpol, press_e, mat3_grad_p_CTR1, mat4_viscous_CTR2,   &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(-1:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR2(jmax,-1:2)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,i_n1,i_p1,i_p2,inod)
      do j = 1, jmax
        i_n1 = j
        inod = j + jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!        i_n2 = i_n1 - jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
!     &                        + mat4_viscous_CTR2(j,-2) * d_vpol(i_n2) &
     &                        - mat3_grad_p_CTR1(-1) *  press_e(i_n1)   &
     &                        + mat4_viscous_CTR2(j,-1) * d_vpol(i_n1)  &
     &                        - mat3_grad_p_CTR1( 0) *  press_e(inod)   &
     &                        + mat4_viscous_CTR2(j, 0) * d_vpol(inod)  &
     &                        - mat3_grad_p_CTR1( 1) *  press_e(i_p1)   &
     &                        + mat4_viscous_CTR2(j, 1) * d_vpol(i_p1)  &
     &                        - mat3_grad_p_CTR1( 2) *  press_e(i_p2)   &
     &                        + mat4_viscous_CTR2(j, 2) * d_vpol(i_p2)
      end do
!$omp end parallel do
!
      end subroutine add_exp4_sph_viscous_CTR2
!
! -----------------------------------------------------------------------
!
      subroutine add_exp4_sph_viscous_CTR1(nnod_rj, jmax,               &
     &          d_vpol, press_e, mat3_grad_p_CTR1, mat4_viscous_CTR1,   &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,0:2)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,i_p1,i_p2,inod)
      do j = 1, jmax
        inod = j
!        i_n1 = inod - jmax
!        i_n2 = i_n1 - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
!     &                        + mat4_viscous_CTR1(j,-2) * d_vpol(i_n2) &
!     &                        - mat3_grad_p_CTR1(-1) *  press_e(i_n1)  &
!     &                        + mat4_viscous_CTR1(j,-1) * d_vpol(i_n1) &
     &                        - mat3_grad_p_CTR1( 0) *  press_e(inod)   &
     &                        + mat4_viscous_CTR1(j, 0) * d_vpol(inod)  &
     &                        - mat3_grad_p_CTR1( 1) *  press_e(i_p1)   &
     &                        + mat4_viscous_CTR1(j, 1) * d_vpol(i_p1)  &
     &                        - mat3_grad_p_CTR1( 2) *  press_e(i_p2)   &
     &                        + mat4_viscous_CTR1(j, 2) * d_vpol(i_p2)
      end do
!$omp end parallel do
!
      end subroutine add_exp4_sph_viscous_CTR1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,               &
     &          mat1_grad_p_CTR1, mat2_viscous_CTR1, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat1_grad_p_CTR1(1:1)
      real(kind = kreal), intent(in) :: mat2_viscous_CTR1(jmax, 0:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       mat7(7,-1,j) = mat7(7,-1,j)
!       mat7(6, 0,j) = mat7(6, 0,j) - mat2_viscous_CTR1(j,-1)
!        mat7(5, 1,j) = mat7(5, 1,j) + mat1_grad_p_CTR1(0)
!
        mat7(4, 2,j) = mat7(4, 2,j) - mat2_viscous_CTR1(j, 0)
!
        mat7(3, 3,j) = mat7(3, 3,j) + mat1_grad_p_CTR1(1)
        mat7(2, 4,j) = mat7(2, 4,j) - mat2_viscous_CTR1(j, 1)
!       mat7(1, 5,j) = mat7(1, 5,j)
      end do
!
      end subroutine sub_sph_pol_viscous_mat7_CTR1
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CTR2(nri, jmax,               &
     &          mat3_grad_p_CTR1, mat4_viscous_CTR2, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1(-1:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR2(jmax,-1:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!       mat9(9, 0,j) = mat9(9, 0,j) - mat4_viscous_CTR2(j,-2)
        mat9(8, 1,j) = mat9(8, 1,j) + mat3_grad_p_CTR1(-1)
        mat9(7, 2,j) = mat9(7, 2,j) - mat4_viscous_CTR2(j,-1)
        mat9(6, 3,j) = mat9(6, 3,j) + mat3_grad_p_CTR1( 0)
!
        mat9(5, 4,j) = mat9(5, 4,j) - mat4_viscous_CTR2(j, 0)
!
        mat9(4, 5,j) = mat9(4, 5,j) + mat3_grad_p_CTR1( 1)
        mat9(3, 6,j) = mat9(3, 6,j) - mat4_viscous_CTR2(j, 1)
        mat9(2, 7,j) = mat9(2, 7,j) + mat3_grad_p_CTR1( 2)
        mat9(1, 8,j) = mat9(1, 8,j) - mat4_viscous_CTR2(j, 2)
      end do
!
      end subroutine sub_sph_pol_viscous_mat9_CTR2
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,               &
     &          mat3_grad_p_CTR1, mat4_viscous_CTR1, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat3_grad_p_CTR1( 0:2)
      real(kind = kreal), intent(in) :: mat4_viscous_CTR1(jmax,0:2)
!
      real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat9(9,-2,j) = mat9(9,-2,j) - mat4_viscous_CTR1(j,-2)
!        mat9(8,-1,j) = mat9(8,-1,j) + mat3_grad_p_CTR1(-1)
!        mat9(7, 0,j) = mat9(7, 0,j) - mat4_viscous_CTR1(j,-1)
        mat9(6, 1,j) = mat9(6, 1,j) + mat3_grad_p_CTR1( 0)
!
        mat9(5, 2,j) = mat9(5, 2,j) - mat4_viscous_CTR1(j, 0)
!
        mat9(4, 3,j) = mat9(4, 3,j) + mat3_grad_p_CTR1( 1)
        mat9(3, 4,j) = mat9(3, 4,j) - mat4_viscous_CTR1(j, 1)
        mat9(2, 5,j) = mat9(2, 5,j) + mat3_grad_p_CTR1( 2)
        mat9(1, 6,j) = mat9(1, 6,j) - mat4_viscous_CTR1(j, 2)
      end do
!
      end subroutine sub_sph_pol_viscous_mat9_CTR1
!
! -----------------------------------------------------------------------
!
      end module set_sph_pol_viscous_CTR
