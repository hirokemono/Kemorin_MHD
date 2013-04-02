!precond_diagonal_scaling_33.f90
!      module precond_diagonal_scaling_33
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_diagonal_33(N, NP, D, ALU_L, ALU_U,           &
!     &     sigma_diag)
!
      module precond_diagonal_scaling_33
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
      subroutine precond_diagonal_33(N, NP, D, ALU_L, ALU_U,            &
     &     sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      real(kind=kreal), intent(in) :: D(9*NP)
      real(kind=kreal), intent(inout) :: ALU_L(9*N)
      real(kind=kreal), intent(inout) :: ALU_U(9*N)
      real(kind=kreal) :: sigma_diag
!
      integer(kind = kint) :: ii
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0

!
!$omp parallel do
      do ii= 1, N
        ALU_L(9*ii-8)= 1.d0 / ( D(9*ii-8)*SIGMA_DIAG )
        ALU_L(9*ii-7)= D(9*ii-7)
        ALU_L(9*ii-6)= D(9*ii-6)
        ALU_L(9*ii-5)= D(9*ii-5)
        ALU_L(9*ii-4)= 1.d0 / ( D(9*ii-4)*SIGMA_DIAG )
        ALU_L(9*ii-3)= D(9*ii-3)
        ALU_L(9*ii-2)= D(9*ii-2)
        ALU_L(9*ii-1)= D(9*ii-1)
        ALU_L(9*ii  )= 1.d0 / ( D(9*ii  )*SIGMA_DIAG )
      end do
!$omp end parallel do
!
!
       end subroutine precond_diagonal_33
!
! ----------------------------------------------------------------------
!
      end module precond_diagonal_scaling_33
