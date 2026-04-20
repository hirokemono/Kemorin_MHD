!>@file   DGEMM_test_omp_target.f90.f90
!!@brief  module DGEMM_test_omp_target.f90
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief Array and loop sizes for DGEMM tests
!!
!!@verbatim
!!      subroutine norm_dgemm_matrix_omp_target(lda, k, A)
!!        integer(kind = kint), intent(in) :: lda, k
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!      subroutine copy_dgemm_matrix_omp_target(lda, k, A_org, A)
!!        integer(kind = kint), intent(in) :: lda, k
!!        real(kind = kreal), intent(in) :: A_org(lda,k)
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!      subroutine copy_dgemm_matrices_omp_target                       &
!!     &         (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!!        integer(kind = kint), intent(in) :: m, n, k
!!        integer(kind = kint), intent(in) :: lda, ldb, ldc
!!        real(kind = kreal), intent(in) :: A_org(lda,k)
!!        real(kind = kreal), intent(in) :: B_org(ldb,n)
!!        real(kind = kreal), intent(in) :: C_org(ldc,n)
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!        real(kind = kreal), intent(inout) :: B(ldb,n)
!!        real(kind = kreal), intent(inout) :: C(ldc,n)
!!
!!      subroutine check_matmul_error(n, ldc, C_ref, C)
!!        integer(kind = kint), intent(in) :: n, ldc
!!        real(kind = kreal), intent(in) :: C_ref(ldc,n)
!!        real(kind = kreal), intent(in) :: C(ldc,n)
!!@endverbatim
!!
      module DGEMM_test_omp_target
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine norm_dgemm_matrix_omp_target(lda, k, A)
!
      integer(kind = kint), intent(in) :: lda, k
!
      real(kind = kreal), intent(inout) :: A(lda,k)
!
      integer(kind = kint) :: i, j
!
!
!$OMP target teams distribute parallel do collapse(2)
      do j = 1, k
        do i = 1, lda
          A(i,j) = 2.0d0 * A(i,j) - 1.0d0
        end do
      end do
!$OMP end target teams distribute parallel do
!
      end subroutine norm_dgemm_matrix_omp_target
!
!  ---------------------------------------------------------------------
!
      subroutine copy_dgemm_matrix_omp_target(lda, k, A_org, A)
!
      integer(kind = kint), intent(in) :: lda, k
      real(kind = kreal), intent(in) :: A_org(lda,k)
      real(kind = kreal), intent(inout) :: A(lda,k)
!
      integer(kind = kint) :: i, j
!
!$OMP target teams distribute parallel do collapse(2)
      do j = 1, k
        do i = 1, lda
          A(i,j) = A_org(i,j)
        end do
      end do
!$OMP end target teams distribute parallel do
!
      end subroutine copy_dgemm_matrix_omp_target
!
!  ---------------------------------------------------------------------
!
      subroutine copy_dgemm_matrices_omp_target                         &
     &         (m, n, k, A_org, lda, B_org, ldb, C_org, ldc, A, B, C)
!
      integer(kind = kint), intent(in) :: m, n, k
      integer(kind = kint), intent(in) :: lda, ldb, ldc
      real(kind = kreal), intent(in) :: A_org(lda,k)
      real(kind = kreal), intent(in) :: B_org(ldb,n)
      real(kind = kreal), intent(in) :: C_org(ldc,n)
!
      real(kind = kreal), intent(inout) :: A(lda,k)
      real(kind = kreal), intent(inout) :: B(ldb,n)
      real(kind = kreal), intent(inout) :: C(ldc,n)
!
      call copy_dgemm_matrix_omp_target(lda, k, A_org, A)
      call copy_dgemm_matrix_omp_target(ldb, n, B_org, B)
      call copy_dgemm_matrix_omp_target(ldc, n, C_org, C)
!
      end subroutine copy_dgemm_matrices_omp_target
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      real(kind = kreal) function sum_matmul_error_omp_target(n, ldc,   &
     &                                                       C_ref, C)
!
      integer(kind = kint), intent(in) :: n, ldc
      real(kind = kreal), intent(in) :: C_ref(ldc,n)
      real(kind = kreal), intent(in) :: C(ldc,n)
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: sum_check
!
! check that C_ref and C are the same entry-wise
      sum_check = 0.0
!$OMP  target teams distribute parallel do                              &
!$omp& reduction(+:sum_check) collapse(2)
      do j = 1, n
        do i = 1, ldc
          sum_check = sum_check + abs(C_ref(i,j) - C(i,j)) / C_ref(i,j)
        end do
      end do
!$OMP end target teams distribute parallel do
      sum_matmul_error_omp_target = sum_check / dble(ldc*n)
!
      end function sum_matmul_error_omp_target
!
!  ---------------------------------------------------------------------
!
      end module DGEMM_test_omp_target
