!
!      module cal_block_ilu_33
!
!     Written by Kemorin
!
!       subroutine cal_bl_ilu_1x33(NP, N, PEsmpTOT, STACKmcG, S, ALU_L)
!       subroutine cal_bl_ilu_2x33(NP, N, PEsmpTOT, STACKmcG,           &
!     &           S1, S2, ALU_L)
!       subroutine cal_bl_ilu_3x33(NP, N, PEsmpTOT, STACKmcG,           &
!     &           S1, S2, S3, ALU_L)
!
!       subroutine cal_bl_ilu_1x33d(NP, N, PEsmpTOT, STACKmcG, S, ALU_L)
!       subroutine cal_bl_ilu_2x33d(NP, N, PEsmpTOT, STACKmcG,          &
!     &           S1, S2, ALU_L)
!       subroutine cal_bl_ilu_3x33d(NP, N, PEsmpTOT, STACKmcG,          &
!     &           S1, S2, S3, ALU_L)
!
      module cal_block_ilu_33
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
       subroutine cal_bl_ilu_1x33(NP, N, PEsmpTOT, STACKmcG, S, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       real(kind = kreal) :: X1, X2, X3
!
      integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S,ALU_L)
!OCL VECTOR, NOVREC
!cdir nodep
        do i= iS, iE
          X1= S(3*i-2)
          X2= S(3*i-1)
          X3= S(3*i  )
          X2= X2 - ALU_L(9*i-5)*X1
          X3= X3 - ALU_L(9*i-2)*X1 - ALU_L(9*i-1)*X2
          X3= ALU_L(9*i  )*  X3
          X2= ALU_L(9*i-4)*( X2 - ALU_L(9*i-3)*X3 )
          X1= ALU_L(9*i-8)*( X1 - ALU_L(9*i-6)*X3 - ALU_L(9*i-7)*X2)
          S(3*i-2) = X1
          S(3*i-1) = X2
          S(3*i  ) = X3
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_1x33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_2x33(NP, N, PEsmpTOT, STACKmcG,            &
     &           S1, S2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
!
      integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S1,S2,ALU_L)
!OCL VECTOR, NOVREC
!cdir nodep
        do i= iS, iE
          S1(3*i-1) = S1(3*i-1) - ALU_L(9*i-5) * S1(3*i-2)
          S2(3*i-1) = S2(3*i-1) - ALU_L(9*i-5) * S2(3*i-2)
!
          S1(3*i  ) = S1(3*i  ) - ALU_L(9*i-2) * S1(3*i-2)              &
     &                          - ALU_L(9*i-1) * S1(3*i-1)
          S2(3*i  ) = S2(3*i  ) - ALU_L(9*i-2) * S2(3*i-2)              &
     &                          - ALU_L(9*i-1) * S2(3*i-1)
!
          S1(3*i  ) = ALU_L(9*i  ) * S1(3*i  )
          S2(3*i  ) = ALU_L(9*i  ) * S2(3*i  )
!
          S1(3*i-1) = ALU_L(9*i-4)                                      &
     &               * ( S1(3*i-1) - ALU_L(9*i-3)*S1(3*i  ) )
          S2(3*i-1) = ALU_L(9*i-4)                                      &
     &               * ( S2(3*i-1) - ALU_L(9*i-3)*S2(3*i  ) )
!
          S1(3*i-2) = ALU_L(9*i-8)                                      &
     &               * ( S1(3*i-2) - ALU_L(9*i-6)*S1(3*i  )             &
     &                             - ALU_L(9*i-7)*S1(3*i-1) )
          S2(3*i-2) = ALU_L(9*i-8)                                      &
     &               * ( S2(3*i-2) - ALU_L(9*i-6)*S2(3*i  )             &
     &                             - ALU_L(9*i-7)*S2(3*i-1) )
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_2x33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_3x33(NP, N, PEsmpTOT, STACKmcG,            &
     &           S1, S2, S3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
      integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!voption indep (S1,S2,S3,ALU_L)
!OCL VECTOR, NOVREC
!cdir nodep
        do i= iS, iE
          S1(3*i-1) = S1(3*i-1) - ALU_L(9*i-5) * S1(3*i-2)
          S2(3*i-1) = S2(3*i-1) - ALU_L(9*i-5) * S2(3*i-2)
          S3(3*i-1) = S3(3*i-1) - ALU_L(9*i-5) * S3(3*i-2)
!
          S1(3*i  ) = S1(3*i  ) - ALU_L(9*i-2) * S1(3*i-2)              &
     &                          - ALU_L(9*i-1) * S1(3*i-1)
          S2(3*i  ) = S2(3*i  ) - ALU_L(9*i-2) * S2(3*i-2)              &
     &                          - ALU_L(9*i-1) * S2(3*i-1)
          S3(3*i  ) = S3(3*i  ) - ALU_L(9*i-2) * S3(3*i-2)              &
     &                          - ALU_L(9*i-1) * S3(3*i-1)
!
          S1(3*i  ) = ALU_L(9*i  ) * S1(3*i  )
          S2(3*i  ) = ALU_L(9*i  ) * S2(3*i  )
          S3(3*i  ) = ALU_L(9*i  ) * S3(3*i  )
!
          S1(3*i-1) = ALU_L(9*i-4)                                      &
     &               * ( S1(3*i-1) - ALU_L(9*i-3)*S1(3*i  ) )
          S2(3*i-1) = ALU_L(9*i-4)                                      &
     &               * ( S2(3*i-1) - ALU_L(9*i-3)*S2(3*i  ) )
          S3(3*i-1) = ALU_L(9*i-4)                                      &
     &               * ( S3(3*i-1) - ALU_L(9*i-3)*S3(3*i  ) )
!
          S1(3*i-2) = ALU_L(9*i-8)                                      &
     &               * ( S1(3*i-2) - ALU_L(9*i-6)*S1(3*i  )             &
     &                             - ALU_L(9*i-7)*S1(3*i-1) )
          S2(3*i-2) = ALU_L(9*i-8)                                      &
     &               * ( S2(3*i-2) - ALU_L(9*i-6)*S2(3*i  )             &
     &                             - ALU_L(9*i-7)*S2(3*i-1) )
          S3(3*i-2) = ALU_L(9*i-8)                                      &
     &               * ( S3(3*i-2) - ALU_L(9*i-6)*S3(3*i  )             &
     &                             - ALU_L(9*i-7)*S3(3*i-1) )
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_1x33d(NP, N, PEsmpTOT, STACKmcG, S, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
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
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,ALU_L)
        do i= iS, iE
          S(3*i-2) = ALU_L(9*i-8) * S(3*i-2)
          S(3*i-1) = ALU_L(9*i-4) * S(3*i-1)
          S(3*i  ) = ALU_L(9*i  ) * S(3*i  )
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_1x33d
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_2x33d(NP, N, PEsmpTOT, STACKmcG,           &
     &     S1, S2, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
!
      integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,ALU_L)
        do i= iS, iE
          S1(3*i-2) = ALU_L(9*i-8) * S1(3*i-2)
          S2(3*i-2) = ALU_L(9*i-8) * S2(3*i-2)
!
          S1(3*i-1) = ALU_L(9*i-4) * S1(3*i-1)
          S2(3*i-1) = ALU_L(9*i-4) * S2(3*i-1)
!
          S1(3*i  ) = ALU_L(9*i  ) * S1(3*i  )
          S2(3*i  ) = ALU_L(9*i  ) * S2(3*i  )
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_2x33d
!
!  ---------------------------------------------------------------------
!
       subroutine cal_bl_ilu_3x33d(NP, N, PEsmpTOT, STACKmcG,           &
     &           S1, S2, S3, ALU_L)
!
       integer(kind=kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind=kint), intent(in):: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: ALU_L(9*N)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
      integer(kind=kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,ALU_L)
        do i= iS, iE
          S1(3*i-2) = ALU_L(9*i-8) * S1(3*i-2)
          S2(3*i-2) = ALU_L(9*i-8) * S2(3*i-2)
          S3(3*i-2) = ALU_L(9*i-8) * S3(3*i-2)
!
          S1(3*i-1) = ALU_L(9*i-4) * S1(3*i-1)
          S2(3*i-1) = ALU_L(9*i-4) * S2(3*i-1)
          S3(3*i-1) = ALU_L(9*i-4) * S3(3*i-1)
!
          S1(3*i  ) = ALU_L(9*i  ) * S1(3*i  )
          S2(3*i  ) = ALU_L(9*i  ) * S2(3*i  )
          S3(3*i  ) = ALU_L(9*i  ) * S3(3*i  )
        enddo
      enddo
!$omp end parallel do
!
      end subroutine cal_bl_ilu_3x33d
!
!  ---------------------------------------------------------------------
!
      end module cal_block_ilu_33
