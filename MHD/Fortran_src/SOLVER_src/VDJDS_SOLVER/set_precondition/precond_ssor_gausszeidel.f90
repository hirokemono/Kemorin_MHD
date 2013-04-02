!precond_ssor_gausszeidel.f90
!      module precond_ssor_gausszeidel
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_gauss_zeidel (N, NP, PEsmpTOT, STACKmcG,      &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!      subroutine precond_ssor (N, NP, PEsmpTOT, STACKmcG,              &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      module precond_ssor_gausszeidel
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
      subroutine precond_gauss_zeidel (N, NP, PEsmpTOT, STACKmcG,       &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
!
      real(kind=kreal), intent(in) :: D(NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(N)
      real(kind=kreal), intent(inout) :: ALU_U(N)
!
      real(kind=kreal), parameter:: one = 1.0d0
!
!
      call precond_ssor (N, NP, PEsmpTOT, STACKmcG,                     &
     &    OtoN_L, OtoN_U, D, ALU_L, ALU_U, one)
!
      end subroutine precond_gauss_zeidel
!
! ----------------------------------------------------------------------
!
      subroutine precond_ssor (N, NP, PEsmpTOT, STACKmcG,               &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
!
      real(kind=kreal), intent(in) :: D(NP)
      real(kind=kreal), intent(in) :: sigma_diag
!
      real(kind=kreal), intent(inout) :: ALU_L(N)
      real(kind=kreal), intent(inout):: ALU_U(N)
!
      integer(kind = kint) :: ip, i, inL, inU
!
!  preconditiong by iLU
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0
!
!cdir parallel do private(i,inL,inU)
!$omp parallel do private(i,inL,inU)
      do ip = 1, PEsmpTOT
!cdir nodep
        do i= STACKmcG(ip-1)+1, STACKmcG(ip)
                 inL  = OtoN_L(i)
                 inU  = OtoN_U(i)
          ALU_L(inL) = 1.d0/(D(i)*sigma_diag)
          ALU_U(inU) = 1.d0/(D(i)*sigma_diag)
        end do
      end  do
!$omp end parallel do
!
       end subroutine precond_ssor
!
! ----------------------------------------------------------------------
!
      end module precond_ssor_gausszeidel
