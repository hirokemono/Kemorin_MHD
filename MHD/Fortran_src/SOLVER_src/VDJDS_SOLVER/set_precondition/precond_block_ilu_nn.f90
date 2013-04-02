!precond_block_ilu_nn.f90
!      module precond_block_ilu_nn
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_bl_ilu_nn(N, NP, NB, PEsmpTOT, STACKmcG,      &
!     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
!
      module precond_block_ilu_nn
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
      subroutine precond_bl_ilu_nn(N, NP, NB, PEsmpTOT, STACKmcG,       &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: NB, PEsmpTOT
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
      integer(kind=kint) :: ip, ii, i, j, k, k1, in0L, in0U
      integer(kind=kint) :: j1, j2, j0, ist, ied
      real(kind=kreal) :: ALUGN(NB*NB*NP)
!
!  preconditiong by Block iLU
!
!
!C== block LU
!
      ALUGN  = 0.d0
      ALU_U= 0.d0
      ALU_L= 0.d0

!cdir parallel do private(ii,i,j,k,k1,j0,j1,j2,ist,ied)
!$omp parallel do private(ii,i,j,k,k1,j0,j1,j2,ist,ied)
      do ip = 1, PEsmpTOT
!
        ist = NB*NB*STACKmcG(ip-1) + 1
        ied = NB*NB*STACKmcG(ip)
        do j= ist, ied
          ALUGN(j)= D(j)
        end do
!
        ist = STACKmcG(ip-1) + 1
        ied = STACKmcG(ip)
        do k1 = 1, NB
!cdir nodep
          do ii= ist, ied
            j = NB*NB*(ii-1) + NB*(k1-1) + k1
            ALUGN(j)= 1.d0 / (D(j)*SIGMA_DIAG)
          end do
        end do
!
        ist = STACKmcG(ip-1) + 1
        ied = STACKmcG(ip)
        do k= 1, NB 
          do i= k+1, NB
!cdir nodep
            do ii= ist, ied
              j = NB*NB*(ii-1) + NB*(k1-1) + k1
              j0 = NB*NB*(ii-1) + NB*(i -1) + k
              ALUGN(j0)= ALUGN(j0) * ALUGN(j)
            end do
!
            do j= k+1, NB
!cdir nodep
              do ii= ist, ied
                j0 = NB*NB*(ii-1) + NB*(i -1) + k
                j1 = NB*NB*(ii-1) + NB*(k -1) + j
                j2 = NB*NB*(ii-1) + NB*(k1-1) + j
                ALUGN(j2)= ALUGN(j2) - ALUGN(j0)*ALUGN(j1)
              end do
            end do
!
          end do
        end do
!
        ist = STACKmcG(ip-1) + 1
        ied = STACKmcG(ip)
        do k = 1, NB*NB
!cdir nodep
          do i= ist, ied
            j =  NB*NB*(i-1) + k
            in0L = NB*NB*(OtoN_L(i)-1) + k
            in0U = NB*NB*(OtoN_U(i)-1) + k
            ALU_L(in0L)= ALUGN(j)
            ALU_U(in0U)= ALUGN(j)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine precond_bl_ilu_nn
!
! ----------------------------------------------------------------------
!
      end module precond_block_ilu_nn
