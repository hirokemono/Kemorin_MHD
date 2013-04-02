!precond_ssor_gausszeidel_nn.f90
!      module precond_ssor_gausszeidel_nn
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_gauss_zeidel_nn(N, NP, NB, PEsmpTOT, STACKmcG,&
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!      subroutine precond_ssor_nn (N, NP, NB, PEsmpTOT, STACKmcG,       &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      module precond_ssor_gausszeidel_nn
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
      subroutine precond_gauss_zeidel_nn(N, NP, NB, PEsmpTOT, STACKmcG, &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: NB
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
!
      real(kind=kreal), intent(in) :: D(NB*NB*NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(NB*NB*N)
      real(kind=kreal), intent(inout) :: ALU_U(NB*NB*N)
!
      real(kind=kreal), parameter:: one = 1.0d0
!
!
      call precond_ssor_nn (N, NP, NB, PEsmpTOT, STACKmcG,              &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, one)
!
      end subroutine precond_gauss_zeidel_nn
!
! ----------------------------------------------------------------------
!
      subroutine precond_ssor_nn (N, NP, NB, PEsmpTOT, STACKmcG,        &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: NB
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
!
      real(kind=kreal), intent(in) :: sigma_diag
      real(kind=kreal), intent(in) :: D(NB*NB*NP)
!
      real(kind=kreal), intent(inout) :: ALU_L(NB*NB*N)
      real(kind=kreal), intent(inout) :: ALU_U(NB*NB*N)
!
      integer(kind = kint) :: ip, i, j, k1, k2, in0L, in0U, ist, ied
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0

!cdir parallel do private(i,in0L,in0U,k1,k2,ist,ied)
!$omp parallel do private(i,in0L,in0U,k1,k2,ist,ied)
      do ip = 1, PEsmpTOT
        ist = STACKmcG(ip-1) + 1
        ied = STACKmcG(ip)
!
        do k1 = 1, NB
          do k2 = 1, NB
!
!cdir nodep
            do i= ist, ied
              j = NB*NB*(i-1) + NB*(k1-1) + k2
              in0L= NB*NB*( OtoN_L(i)-1 ) + NB*(k1-1) + k2
              in0U= NB*NB*( OtoN_U(i)-1 ) + NB*(k1-1) + k2
!
              ALU_L(in0L)= D(j)
              ALU_U(in0U)= D(j)
            enddo
          enddo
        enddo
!
        do k1 = 1, NB
!cdir nodep
          do i= ist, ied
            in0L= NB*NB*( OtoN_L(i)-1 ) + NB*(k1-1) + k1
            in0U= NB*NB*( OtoN_U(i)-1 ) + NB*(k1-1) + k1
!
            ALU_L(in0L) = 1.d0/ ( ALU_L(in0L)*SIGMA_DIAG )
            ALU_U(in0U) = 1.d0/ ( ALU_U(in0U)*SIGMA_DIAG )
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine precond_ssor_nn
!
! ----------------------------------------------------------------------
!
      end module precond_ssor_gausszeidel_nn
