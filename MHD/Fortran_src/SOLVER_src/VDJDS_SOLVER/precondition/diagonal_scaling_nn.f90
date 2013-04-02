!
!      module diagonal_scaling_nn
!
!     Written by Kemorin
!
!       subroutine diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,     &
!     &           S, V, ALU_L)
!       subroutine diag_scaling_2xnn(NP, N, NB, PEsmpTOT, STACKmcG,     &
!     &           S1, S2, V1, V2, ALU_L)
!       subroutine diag_scaling_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,     &
!     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
      module diagonal_scaling_nn
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
       subroutine diag_scaling_1xnn(NP, N, NB, PEsmpTOT, STACKmcG,      &
     &           S, V, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            ii =   NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S(ii)= V(ii) * ALU_L(im)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_1xnn
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_2xnn(NP, N, NB, PEsmpTOT, STACKmcG,      &
     &           S1, S2, V1, V2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!voption indep (S1,S2,V1,V2)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            ii =   NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii)= V1(ii) * ALU_L(im)
            S2(ii)= V2(ii) * ALU_L(im)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_2xnn
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_3xnn(NP, N, NB, PEsmpTOT, STACKmcG,      &
     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(in) :: ALU_L(NB*NB*N)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer(kind=kint) :: ip, iS, iE, i, k1, ii, im
!
!
!$omp parallel do private(iS,iE,i,ii,im,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!voption indep (S1,S2,S3,V1,V2,V3)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            ii =   NB*(i-1) + k1
            im = NB*NB*(i-1) + NB*(k1-1) + k1
            S1(ii)= V1(ii) * ALU_L(im)
            S2(ii)= V2(ii) * ALU_L(im)
            S3(ii)= V3(ii) * ALU_L(im)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_3xnn
!
!  ---------------------------------------------------------------------
!
      end module diagonal_scaling_nn
