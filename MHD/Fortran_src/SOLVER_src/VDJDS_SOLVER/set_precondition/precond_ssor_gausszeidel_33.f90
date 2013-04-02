!precond_ssor_gausszeidel_33.f90
!      module precond_ssor_gausszeidel_33
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_gauss_zeidel_33 (N, NP, PEsmpTOT, STACKmcG,   &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!      subroutine precond_ssor_33 (N, NP, PEsmpTOT, STACKmcG,           &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      module precond_ssor_gausszeidel_33
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
      subroutine precond_gauss_zeidel_33 (N, NP, PEsmpTOT, STACKmcG,    &
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
      real(kind=kreal), intent(in) :: D(9*NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(9*N)
      real(kind=kreal), intent(inout) :: ALU_U(9*N)
!
      real(kind=kreal), parameter:: one = 1.0d0
!
!
      call precond_ssor_33 (N, NP, PEsmpTOT, STACKmcG,                  &
     &    OtoN_L, OtoN_U, D, ALU_L, ALU_U, one)
!
      end subroutine precond_gauss_zeidel_33
!
! ----------------------------------------------------------------------
!
      subroutine precond_ssor_33 (N, NP, PEsmpTOT, STACKmcG,            &
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
      real(kind=kreal), intent(in) :: sigma_diag
      real(kind=kreal), intent(in) :: D(9*NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(9*N)
      real(kind=kreal), intent(inout) :: ALU_U(9*N)
!
      integer(kind = kint) :: ip, i, in0L, in0U
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0

!cdir parallel do private(i,in0L,in0U)
!$omp parallel do private(i,in0L,in0U)
      do ip = 1, PEsmpTOT
!cdir nodep
        do i= STACKmcG(ip-1)+1, STACKmcG(ip)
          in0L= OtoN_L(i)
          in0U= OtoN_U(i)
!
          ALU_L(9*in0L-8)= 1.d0/ ( D(9*i-8)*SIGMA_DIAG )
          ALU_L(9*in0L-7)= D(9*i-7)
          ALU_L(9*in0L-6)= D(9*i-6)
!
          ALU_L(9*in0L-5)= D(9*i-5)
          ALU_L(9*in0L-4)= 1.d0/ ( D(9*i-4)*SIGMA_DIAG )
          ALU_L(9*in0L-3)= D(9*i-3)
!
          ALU_L(9*in0L-2)= D(9*i-2)
          ALU_L(9*in0L-1)= D(9*i-1)
          ALU_L(9*in0L  )= 1.d0/ ( D(9*i  )*SIGMA_DIAG )
!
!
          ALU_U(9*in0U-8)= 1.d0/ ( D(9*i-8)*SIGMA_DIAG )
          ALU_U(9*in0U-7)= D(9*i-7)
          ALU_U(9*in0U-6)= D(9*i-6)
!
          ALU_U(9*in0U-5)= D(9*i-5)
          ALU_U(9*in0U-4)= 1.d0/ ( D(9*i-4)*SIGMA_DIAG )
          ALU_U(9*in0U-3)= D(9*i-3)
!
          ALU_U(9*in0U-2)= D(9*i-2)
          ALU_U(9*in0U-1)= D(9*i-1)
          ALU_U(9*in0U  )= 1.d0/ ( D(9*i  )*SIGMA_DIAG )
!
        end do
      end do
!$omp end parallel do
!
!
       end subroutine precond_ssor_33
!
! ----------------------------------------------------------------------
!
      end module precond_ssor_gausszeidel_33
