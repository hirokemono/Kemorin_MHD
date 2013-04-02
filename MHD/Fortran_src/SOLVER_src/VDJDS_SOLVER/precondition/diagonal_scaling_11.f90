!
!      module diagonal_scaling_11
!
!     Written by Kemorin
!
!       subroutine diag_scaling_1x11(NP, N, PEsmpTOT, STACKmcG,         &
!     &           S, V, ALU_L)
!       subroutine diag_scaling_2x11(NP, N, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, V1, V2, ALU_L)
!       subroutine diag_scaling_3x11(NP, N, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
      module diagonal_scaling_11
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
       subroutine diag_scaling_1x11(NP, N, PEsmpTOT, STACKmcG,          &
     &           S, V, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            S(i)= V(i) * ALU_L(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_1x11
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_2x11(NP, N, PEsmpTOT, STACKmcG,          &
     &           S1, S2, V1, V2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V1(NP), V2(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP)
!
       integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S1,S2,V1,V2)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            S1(i)= V1(i) * ALU_L(i)
            S2(i)= V2(i) * ALU_L(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_2x11
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_3x11(NP, N, PEsmpTOT, STACKmcG,          &
     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V1(NP), V2(NP)
       real(kind = kreal), intent(in) :: V3(NP)
       real(kind = kreal), intent(in) :: ALU_L(N)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP)
       real(kind = kreal), intent(inout) :: S3(NP)
!
       integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S1,S2,S3,V1,V2,V3)
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
          do i= iS, iE
            S1(i)= V1(i) * ALU_L(i)
            S2(i)= V2(i) * ALU_L(i)
            S3(i)= V3(i) * ALU_L(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_3x11
!
!  ---------------------------------------------------------------------
!
      end module diagonal_scaling_11
