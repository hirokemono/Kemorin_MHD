!>@file   calypso_DGEMM.f90
!!@brief  module calypso_DGEMM
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief Simple Matrix-matrix product (DGEMM)
!!
!!@verbatim
!!      subroutine calypso_simple_dgemm                                 &
!!     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!!      subroutine calypso_dgemm_openmp                                 &
!!     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!!        integer(c_int), intent(in) :: m, n, k
!!        integer(c_int), intent(in) :: lda, ldb, ldc
!!        real(c_double), intent(in) :: alpha, beta
!!        real(kind = kreal), intent(in), target :: A(lda,k)
!!        real(kind = kreal), intent(in), target :: B(ldb,n)
!!        real(kind = kreal), intent(inout), target :: C(ldc,n)
!!@endverbatim
      module calypso_DGEMM
!
      use iso_c_binding
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
      subroutine calypso_simple_dgemm                                   &
     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!
      integer(c_int), intent(in) :: m, n, k
      integer(c_int), intent(in) :: lda, ldb, ldc
      real(c_double), intent(in) :: alpha, beta
      real(kind = kreal), intent(in), target :: A(lda,k)
      real(kind = kreal), intent(in), target :: B(ldb,n)
!
      real(kind = kreal), intent(inout), target :: C(ldc,n)
!
      integer(kind = kint) :: i, j, ij
!
!
      do j = 1, n
        do i = 1, m
          C(i,j) = beta * C(i,j)
!
          do ij = 1, k
            C(i,j) = C(i,j) + alpha * A(i,ij) * B(ij,j)
          end do
        end do
      end do
!
      end subroutine calypso_simple_dgemm
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_dgemm_openmp                                   &
     &         (m, n, k, alpha, A, lda, B, ldb, beta, C, ldc)
!
      integer(c_int), intent(in) :: m, n, k
      integer(c_int), intent(in) :: lda, ldb, ldc
      real(c_double), intent(in) :: alpha, beta
      real(kind = kreal), intent(in), target :: A(lda,k)
      real(kind = kreal), intent(in), target :: B(ldb,n)
!
      real(kind = kreal), intent(inout), target :: C(ldc,n)
!
      integer(kind = kint) :: i, j, ij
!
!
!$OMP parallel do collapse(2)
      do j = 1, n
        do i = 1, m
          C(i,j) = beta * C(i,j)
!
          do ij = 1, k
            C(i,j) = C(i,j) + alpha * A(i,ij) * B(ij,j)
          end do
        end do
      end do
!$OMP end parallel do
!
      end subroutine calypso_dgemm_openmp
!
!  ---------------------------------------------------------------------
!
      end module calypso_DGEMM
