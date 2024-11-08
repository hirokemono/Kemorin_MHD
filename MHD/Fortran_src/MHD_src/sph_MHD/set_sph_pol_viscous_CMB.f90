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
!!      subroutine add_exp2_sph_pol_viscous_CMB                         &
!!     &         (k_CMB, nnod_rj, nri, jmax,                            &
!!     &          mat1_grad_p_CMB, mat2_viscous_CMB,                    &
!!     &          d_vpol, press_e, d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CMB(0:0)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp4_sph_pol_viscous_CMB                         &
!!     &         (k_CMB, nnod_rj, nri, jmax,                            &
!!     &          mat3_grad_p_CMB1, mat3_grad_p_CMB,                    &
!!     &          mat4_viscous_CMB1, mat4_viscous_CMB,                  &
!!     &          d_vpol, press_e, d_viscous_p)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(-1:1)
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB(-1:0)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!
!!      subroutine sub_sph_pol_viscous_mat7_CMB1(k_CMB, nri, jmax,      &
!!     &          mat1_grad_p_CMB1, mat2_viscous_CMB1, mat7)
!!      subroutine sub_sph_pol_viscous_mat7_CMB(k_CMB, nri, jmax, mat7)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat1_grad_p_CMB1( 0:1)
!!        real(kind = kreal), intent(in) :: mat2_viscous_CMB1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_pol_viscous_mat9_CMB(k_CMB, nri, jmax,       &
!!     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(-1:1)
!!        real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
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
      subroutine add_exp2_sph_pol_viscous_CMB                           &
     &         (k_CMB, nnod_rj, nri, jmax,                              &
     &          mat1_grad_p_CMB, mat2_viscous_CMB,                      &
     &          d_vpol, press_e, d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p_CMB(0:0)
      real(kind = kreal), intent(in) :: mat2_viscous_CMB(jmax,-1:0)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod
!
!
!$omp parallel do private(j,inod,i_n1)
      do j = 1, jmax
        inod = j + (k_CMB-1) * nri
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
!
      subroutine add_exp4_sph_pol_viscous_CMB                           &
     &         (k_CMB, nnod_rj, nri, jmax,                              &
     &          mat3_grad_p_CMB1, mat3_grad_p_CMB,                      &
     &          mat4_viscous_CMB1, mat4_viscous_CMB,                    &
     &          d_vpol, press_e, d_viscous_p)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(-1:1)
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB(-1:0)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB(jmax,-2:0)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_n2, inod
!
!
!$omp parallel do private(j,inod,i_n1,i_n2,i_p1)
      do j = 1, jmax
        inod = j + (k_CMB-2) * nri
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                   + mat4_viscous_CMB1(j,-2) * d_vpol(i_n2)       &
     &                   - mat3_grad_p_CMB1(-1) *  press_e(i_n1)        &
     &                   + mat4_viscous_CMB1(j,-1) * d_vpol(i_n1)       &
     &                   - mat3_grad_p_CMB1( 0) *  press_e(inod)        &
     &                   + mat4_viscous_CMB1(j, 0) * d_vpol(inod)       &
     &                   - mat3_grad_p_CMB1( 1) *  press_e(i_p1)        &
     &                   + mat4_viscous_CMB1(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
!$omp parallel do private(j,inod,i_n1,i_n2)
      do j = 1, jmax
        inod = j + (k_CMB-1) * nri
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
     &                   + mat4_viscous_CMB(j,-2) * d_vpol(i_n2)        &
     &                   - mat3_grad_p_CMB(-1) *  press_e(i_n1)         &
     &                   + mat4_viscous_CMB(j,-1) * d_vpol(i_n1)        &
     &                   - mat3_grad_p_CMB( 0) *  press_e(inod)         &
     &                   + mat4_viscous_CMB(j, 0) * d_vpol(inod)
      end do
!$omp end parallel do
!
      end subroutine add_exp4_sph_pol_viscous_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat7_CMB1(k_CMB, nri, jmax,        &
     &          mat1_grad_p_CMB1, mat2_viscous_CMB1, mat7)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: mat1_grad_p_CMB1( 0:1)
      real(kind = kreal), intent(in) :: mat2_viscous_CMB1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat7(7,2*k_CMB-5,j) = mat7(7,2*k_CMB-5,j)
        mat7(6,2*k_CMB-4,j) = mat7(6,2*k_CMB-4,j)                       &
     &                       - mat2_viscous_CMB1(j,-1)
        mat7(5,2*k_CMB-3,j) = mat7(5,2*k_CMB-3,j)                       &
     &                       + mat1_grad_p_CMB1(0)
!
        mat7(4,2*k_CMB-2,j) = mat7(4,2*k_CMB-2,j)                       &
     &                       - mat2_viscous_CMB1(j, 0)
!
        mat7(3,2*k_CMB-1,j) = mat7(3,2*k_CMB-1,j)                       &
     &                       + mat1_grad_p_CMB1(1)
        mat7(2,2*k_CMB,  j) = mat7(2,2*k_CMB,  j)                       &
     &                       - mat2_viscous_CMB1(j, 1)
        if(2*k_CMB+1 .le. 2*nri) mat7(1,2*k_CMB+1,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat7_CMB1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat7_CMB(k_CMB, nri, jmax, mat7)
!
      integer(kind = kint), intent(in) :: k_CMB, nri, jmax
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat7(7,2*k_CMB-3,j) = zero
        mat7(6,2*k_CMB-2,j) = zero
        mat7(5,2*k_CMB-1,j) = zero
!
        mat7(4,2*k_CMB,  j) = one
!
       if(2*k_CMB+1 .le. 2*nri)  mat7(3,2*k_CMB+1,j) = zero
       if(2*k_CMB+2 .le. 2*nri)  mat7(2,2*k_CMB+2,j) = zero
       if(2*k_CMB+3 .le. 2*nri)  mat7(1,2*k_CMB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat7_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CMB(k_CMB, nri, jmax,         &
     &          mat3_grad_p_CMB1, mat4_viscous_CMB1, mat9)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: mat3_grad_p_CMB1(-1:1)
      real(kind = kreal), intent(in) :: mat4_viscous_CMB1(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat9(9,2*k_CMB-6,j) = mat9(9,2*k_CMB-6,j)                       &
     &                       - mat4_viscous_CMB1(j,-2)
        mat9(8,2*k_CMB-5,j) = mat9(8,2*k_CMB-5,j)                       &
     &                       + mat3_grad_p_CMB1(-1)
        mat9(7,2*k_CMB-4,j) = mat9(7,2*k_CMB-4,j)                       &
     &                       - mat4_viscous_CMB1(j,-1)
        mat9(6,2*k_CMB-3,j) = mat9(6,2*k_CMB-3,j)                       &
     &                       + mat3_grad_p_CMB1(0)
!
        mat9(5,2*k_CMB-2,j) = mat9(5,2*k_CMB-2,j)                       &
     &                       - mat4_viscous_CMB1(j, 0)
!
        mat9(4,2*k_CMB-1,j) = mat9(4,2*k_CMB-1,j)                       &
     &                       + mat3_grad_p_CMB1(1)
        mat9(3,2*k_CMB,  j) = mat9(3,2*k_CMB,  j)                       &
     &                       - mat4_viscous_CMB1(j, 1)
        if(2*k_CMB+1 .le. 2*nri) mat9(2,2*k_CMB+1,j) = zero
        if(2*k_CMB+2 .le. 2*nri) mat9(1,2*k_CMB+2,j) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat9(9,2*k_CMB-4,j) = zero
        mat9(8,2*k_CMB-3,j) = zero
        mat9(7,2*k_CMB-2,j) = zero
        mat9(6,2*k_CMB-1,j) = zero
!
        mat9(5,2*k_CMB,  j) = one
!
       if(2*k_CMB+1 .le. 2*nri)  mat9(4,2*k_CMB+1,j) = zero
       if(2*k_CMB+2 .le. 2*nri)  mat9(3,2*k_CMB+2,j) = zero
       if(2*k_CMB+3 .le. 2*nri)  mat9(2,2*k_CMB+3,j) = zero
       if(2*k_CMB+4 .le. 2*nri)  mat9(1,2*k_CMB+4,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_pol_viscous_mat9_CMB
!
! -----------------------------------------------------------------------
!
      end module set_sph_pol_viscous_CMB

