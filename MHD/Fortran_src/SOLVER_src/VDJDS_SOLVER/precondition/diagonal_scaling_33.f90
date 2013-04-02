!
!      module diagonal_scaling_33
!
!     Written by Kemorin
!
!       subroutine diag_scaling_1x33(NP, N, PEsmpTOT, STACKmcG, S, V,   &
!     &           ALU_L)
!       subroutine diag_scaling_2x33(NP, N, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, V1, V2, ALU_L)
!       subroutine diag_scaling_3x33(NP, N, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
      module diagonal_scaling_33
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
       subroutine diag_scaling_1x33(NP, N, PEsmpTOT, STACKmcG, S, V,    &
     &           ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S(3*NP)
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
!cdir nodep
        do i= iS, iE
          S(3*i-2)= V(3*i-2) * ALU_L(9*i-8)
          S(3*i-1)= V(3*i-1) * ALU_L(9*i-4)
          S(3*i  )= V(3*i  ) * ALU_L(9*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_1x33
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_2x33(NP, N, PEsmpTOT, STACKmcG,          &
     &           S1, S2, V1, V2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind=kreal), intent(in) :: V1(3*NP), V2(3*NP)
       real(kind=kreal), intent(in) :: ALU_L(9*N)
       real(kind=kreal), intent(inout) :: S1(3*NP), S2(3*NP)
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
!cdir nodep
        do i= iS, iE
          S1(3*i-2)= V1(3*i-2) * ALU_L(9*i-8)
          S2(3*i-2)= V2(3*i-2) * ALU_L(9*i-8)
!
          S1(3*i-1)= V1(3*i-1) * ALU_L(9*i-4)
          S2(3*i-1)= V2(3*i-1) * ALU_L(9*i-4)
!
          S1(3*i  )= V1(3*i  ) * ALU_L(9*i  )
          S2(3*i  )= V2(3*i  ) * ALU_L(9*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_2x33
!
!  ---------------------------------------------------------------------
!
       subroutine diag_scaling_3x33(NP, N, PEsmpTOT, STACKmcG,          &
     &           S1, S2, S3, V1, V2, V3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

       real(kind=kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind=kreal), intent(in) :: ALU_L(9*N)
       real(kind=kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
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
!cdir nodep
        do i= iS, iE
          S1(3*i-2)= V1(3*i-2) * ALU_L(9*i-8)
          S2(3*i-2)= V2(3*i-2) * ALU_L(9*i-8)
          S3(3*i-2)= V3(3*i-2) * ALU_L(9*i-8)
!
          S1(3*i-1)= V1(3*i-1) * ALU_L(9*i-4)
          S2(3*i-1)= V2(3*i-1) * ALU_L(9*i-4)
          S3(3*i-1)= V3(3*i-1) * ALU_L(9*i-4)
!
          S1(3*i  )= V1(3*i  ) * ALU_L(9*i  )
          S2(3*i  )= V2(3*i  ) * ALU_L(9*i  )
          S3(3*i  )= V3(3*i  ) * ALU_L(9*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine diag_scaling_3x33
!
!  ---------------------------------------------------------------------
!
      end module diagonal_scaling_33
