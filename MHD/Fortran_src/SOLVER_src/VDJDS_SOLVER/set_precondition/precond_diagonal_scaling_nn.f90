!precond_diagonal_scaling_nn.f90
!      module precond_diagonal_scaling_nn
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_diagonal_nn (N, NP, NB, PEsmpTOT, STACKmcG,   &
!     &          D, ALU_L, ALU_U, sigma_diag)
!
      module precond_diagonal_scaling_nn
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
      subroutine precond_diagonal_nn (N, NP, NB, PEsmpTOT, STACKmcG,    &
     &          D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: NB, PEsmpTOT
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
      real(kind=kreal), intent(in) :: D(NB*NB*NP)
      real(kind=kreal), intent(inout) :: ALU_L(NB*NB*N)
      real(kind=kreal), intent(inout) :: ALU_U(NB*NB*N)
      real(kind=kreal) :: sigma_diag
!
      integer(kind = kint) :: ip, ii, j, k1, ist, ied
!
!
      ALU_U= 0.d0
      ALU_L= 0.d0
!
!cdir parallel do private(ii,j,k1,ist,ied)
!$omp parallel do private(ii,j,k1,ist,ied)
      do ip = 1, PEsmpTOT
        ist = NB*NB*STACKmcG(ip-1) + 1
        ied = NB*NB*STACKmcG(ip)
!cdir nodep
        do j= ist, ied
          ALU_L(j) = D(j)
        end do

        ist = STACKmcG(ip-1) + 1
        ied = STACKmcG(ip)
        do k1 = 1, NB
!cdir nodep
          do ii= ist, ied
            j = NB*NB*(ii-1) + NB*(k1-1) + k1
            ALU_L(j) = 1.d0 / ( ALU_L(j)*SIGMA_DIAG )
          end do
        end do
      end do
!$omp end parallel do

       end subroutine precond_diagonal_nn
!
! ----------------------------------------------------------------------
!
      end module precond_diagonal_scaling_nn
