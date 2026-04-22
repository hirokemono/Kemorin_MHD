!>@file   DGEMM_test_openmp.f90
!!@brief  module DGEMM_test_openmp
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief Array and loop sizes for DGEMM tests
!!
!!@verbatim
!!      real(kind = kreal) function sum_matmul_error(n, ldc, C_ref, C)
!!        integer(kind = kint), intent(in) :: n, ldc
!!        real(kind = kreal), intent(in) :: C_ref(ldc,n)
!!        real(kind = kreal), intent(in) :: C(ldc,n)
!!      subroutine check_matmul_error(sum_check)
!!        real(kind = kreal), intent(in) :: sum_check
!!
!!      subroutine norm_dgemm_matrices(m, n, k, A, lda, B, ldb, C, ldc)
!!        integer(kind = kint), intent(in) :: m, n, k
!!        integer(kind = kint), intent(in) :: lda, ldb, ldc
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!        real(kind = kreal), intent(inout) :: B(ldb,n)
!!        real(kind = kreal), intent(inout) :: C(ldc,n)
!!      subroutine copy_dgemm_matrices(m, n, k, A_org, lda, B_org, ldb, &
!!     &                               C_org, ldc, A, B, C)
!!        integer(kind = kint), intent(in) :: m, n, k
!!        integer(kind = kint), intent(in) :: lda, ldb, ldc
!!        real(kind = kreal), intent(in) :: A_org(lda,k)
!!        real(kind = kreal), intent(in) :: B_org(ldb,n)
!!        real(kind = kreal), intent(in) :: C_org(ldc,n)
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!        real(kind = kreal), intent(inout) :: B(ldb,n)
!!        real(kind = kreal), intent(inout) :: C(ldc,n)
!!
!!      subroutine norm_dgemm_matrix(lda, k, A)
!!        integer(kind = kint), intent(in) :: lda, k
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!      subroutine copy_dgemm_matrix(lda, k, A_org, A)
!!        integer(kind = kint), intent(in) :: lda, k
!!        real(kind = kreal), intent(in) :: A_org(lda,k)
!!        real(kind = kreal), intent(inout) :: A(lda,k)
!!@endverbatim
!!
      module DGEMM_test_openmp
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
      real(kind = kreal) function sum_matmul_error(n, ldc, C_ref, C)
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
!$OMP parallel do reduction(+:sum_check) collapse(2)
      do j = 1, n
        do i = 1, ldc
          sum_check = sum_check + abs(C_ref(i,j) - C(i,j)) / C_ref(i,j)
        end do
      end do
!$OMP end parallel do
      sum_matmul_error = sum_check / dble(ldc*n)
!
      end function sum_matmul_error
!
!  ---------------------------------------------------------------------
!
      subroutine check_matmul_error(sum_check)
!
      real(kind = kreal), intent(in) :: sum_check
!
!
      if (abs(sum_check) < 1.e-13) then
         write (*, '(a)',advance="no") "PASSED!: "
      else
         write (*, '(a)',advance="no") "FAILED!: "
      end if
      write (*, '(a,1pE16.6e3)') "Average error = ", sum_check
!
      end subroutine check_matmul_error
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine norm_dgemm_matrices(m, n, k, A, lda, B, ldb, C, ldc)
!
      integer(kind = kint), intent(in) :: m, n, k
      integer(kind = kint), intent(in) :: lda, ldb, ldc
      real(kind = kreal), intent(inout) :: A(lda,k)
      real(kind = kreal), intent(inout) :: B(ldb,n)
      real(kind = kreal), intent(inout) :: C(ldc,n)
!
      call norm_dgemm_matrix(lda, k, A)
      call norm_dgemm_matrix(ldb, n, B)
      call norm_dgemm_matrix(ldc, n, C)
!
      end subroutine norm_dgemm_matrices
!
!  ---------------------------------------------------------------------
!
      subroutine copy_dgemm_matrices(m, n, k, A_org, lda, B_org, ldb,   &
     &                               C_org, ldc, A, B, C)
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
      call copy_dgemm_matrix(lda, k, A_org, A)
      call copy_dgemm_matrix(ldb, n, B_org, B)
      call copy_dgemm_matrix(ldc, n, C_org, C)
!
      end subroutine copy_dgemm_matrices
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine norm_dgemm_matrix(lda, k, A)
!
      integer(kind = kint), intent(in) :: lda, k
!
      real(kind = kreal), intent(inout) :: A(lda,k)
!
      integer(kind = kint) :: i, j
!
!
!$OMP parallel do collapse(2)
      do j = 1, k
        do i = 1, lda
          A(i,j) = 2.0d0 * A(i,j) - 1.0d0
        end do
      end do
!$OMP end parallel do
!
      end subroutine norm_dgemm_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine copy_dgemm_matrix(lda, k, A_org, A)
!
      integer(kind = kint), intent(in) :: lda, k
      real(kind = kreal), intent(in) :: A_org(lda,k)
      real(kind = kreal), intent(inout) :: A(lda,k)
!
      integer(kind = kint) :: i, j
!
!$OMP parallel do collapse(2)
      do j = 1, k
        do i = 1, lda
          A(i,j) = A_org(i,j)
        end do
      end do
!$OMP end parallel do
!
      end subroutine copy_dgemm_matrix
!
!  ---------------------------------------------------------------------
!
      end module DGEMM_test_openmp
