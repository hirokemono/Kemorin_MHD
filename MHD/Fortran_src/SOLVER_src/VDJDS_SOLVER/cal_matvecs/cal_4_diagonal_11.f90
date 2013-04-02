!
!      module cal_4_diagonal_11
!
!     Written by Kemorin
!
!       subroutine set_by_diagonal_11(NP, PEsmpTOT, STACKmcG,           &
!     &           S, V, D)
!       subroutine set_by_diagonal_3x11(NP, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, S3, V1, V2, V3, D)
!
!       subroutine subtract_diagonal_11(NP, PEsmpTOT, STACKmcG,         &
!     &           S, B, V, D)
!       subroutine subtract_diagonal_3x11(NP, PEsmpTOT, STACKmcG,       &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
!       subroutine add_diagonal_11(NP, PEsmpTOT, STACKmcG,              &
!     &           S, B, V, D)
!       subroutine add_diagonal_3x11(NP, PEsmpTOT, STACKmcG,            &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
      module cal_4_diagonal_11
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
       subroutine set_by_diagonal_11(NP, PEsmpTOT, STACKmcG, S, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep noloopchg
!voption indep (S,V,D)
        do i= iS, iE
          S(i) = D(i)*V(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine set_by_diagonal_11
!
!  ---------------------------------------------------------------------
!
       subroutine set_by_diagonal_3x11(NP, PEsmpTOT, STACKmcG,          &
     &           S1, S2, S3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,D)
        do i= iS, iE
          S1(i) = D(i)*V1(i)
          S2(i) = D(i)*V2(i)
          S3(i) = D(i)*V3(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine set_by_diagonal_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_11(NP, PEsmpTOT, STACKmcG,          &
     &           S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT      )
!
       real(kind = kreal), intent(in) :: V(NP), B(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep noloopchg
!voption indep (S,V,B,D)
        do i= iS, iE
          S(i) = B(i) - D(i) * V(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine subtract_diagonal_11
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_3x11(NP, PEsmpTOT, STACKmcG,        &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT      )
!
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: V1(NP), B1(NP)
       real(kind = kreal), intent(in) :: V2(NP), B2(NP)
       real(kind = kreal), intent(in) :: V3(NP), B3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,B1,B2,B3,D)
        do i= iS, iE
          S1(i) = B1(i) - D(i) * V1(i)
          S2(i) = B2(i) - D(i) * V2(i)
          S3(i) = B3(i) - D(i) * V3(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine subtract_diagonal_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_11(NP, PEsmpTOT, STACKmcG,               &
     &           S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT      )
!
       real(kind = kreal), intent(in) :: V(NP), B(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep noloopchg
!voption indep (S,V,B,D)
        do i= iS, iE
          S(i) = B(i) + D(i) * V(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine add_diagonal_11
!
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_3x11(NP, PEsmpTOT, STACKmcG,             &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT      )
!
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: V1(NP), B1(NP)
       real(kind = kreal), intent(in) :: V2(NP), B2(NP)
       real(kind = kreal), intent(in) :: V3(NP), B3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,B1,B2,B3,D)
        do i= iS, iE
          S1(i) = B1(i) + D(i) * V1(i)
          S2(i) = B2(i) + D(i) * V2(i)
          S3(i) = B3(i) + D(i) * V3(i)
        end do
      end do
!$omp end parallel do
!
       end subroutine add_diagonal_3x11
!
!  ---------------------------------------------------------------------
!
      end module cal_4_diagonal_11
