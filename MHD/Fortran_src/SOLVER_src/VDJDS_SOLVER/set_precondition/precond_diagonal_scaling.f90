!precond_diagonal_scaling.f90
!      module precond_diagonal_scaling
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_diagonal(N, NP, D, ALU_L, ALU_U, sigma_diag)
!
      module precond_diagonal_scaling
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine precond_diagonal(N, NP, D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      real(kind=kreal), intent(in) :: D(NP)
      real(kind=kreal), intent(inout) :: ALU_L(N)
      real(kind=kreal), intent(inout) :: ALU_U(N)
      real(kind=kreal) :: sigma_diag
!
      integer (kind = kint) :: inod
!
!  preconditiong by diagonal scaling
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0
!
!$omp parallel do
        do inod = 1, N
          ALU_L(inod) = 1.d0/(D(inod)*sigma_diag)
        end do
!$omp end parallel do
!
       end subroutine precond_diagonal
!
! ----------------------------------------------------------------------
!
      end module precond_diagonal_scaling
