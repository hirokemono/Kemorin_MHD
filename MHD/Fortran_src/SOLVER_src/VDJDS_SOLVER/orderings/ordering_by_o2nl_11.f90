!
!      module ordering_by_o2nl_11
!
!     Written by Kemorin
!
!       subroutine ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X, A)
!       subroutine ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, A1, A2)
!       subroutine ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!       subroutine ordering_1x4_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!       subroutine ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,           &
!     &           A4, A5, A6)
!
!       subroutine ordering_int_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, iX, iA)
!
      module ordering_by_o2nl_11
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
       subroutine ordering_1x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X, A)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A(NP)
       real(kind=kreal), intent(inout) :: X(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A,X,OtoN_L,STACKmcG) tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A)
!CDIR ON_ADB(X)
!cdir nodep
!voption indep (A,X,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            X(in1)= A(i)
          enddo
      enddo
!$omp end parallel do

       end subroutine ordering_1x1_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, A1, A2)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NP), A2(NP)
       real(kind=kreal), intent(inout) :: X1(NP), X2(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A1,A2,X1,X2,OtoN_L,STACKmcG) tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A1)
!CDIR ON_ADB(A2)
!CDIR ON_ADB(X1)
!CDIR ON_ADB(X2)
!cdir nodep
!voption indep (A1,A2,X1,X2,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
          end do
      enddo
!$omp end parallel do

       end subroutine ordering_1x2_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NP), A2(NP)
       real(kind=kreal), intent(in) :: A3(NP)
       real(kind=kreal), intent(inout) :: X1(NP), X2(NP)
       real(kind=kreal), intent(inout) :: X3(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A1,A2,A3,X1,X2,X3,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A1)
!CDIR ON_ADB(A2)
!CDIR ON_ADB(A3)
!CDIR ON_ADB(X1)
!CDIR ON_ADB(X2)
!CDIR ON_ADB(X3)
!cdir nodep
!voption indep (A1,A2,A3,X1,X2,X3,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
            X3(in1)= A3(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine ordering_1x3_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x4_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NP), A2(NP), A3(NP), A4(NP)
       real(kind=kreal), intent(inout) :: X1(NP), X2(NP), X3(NP), X4(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A1,A2,A3,A4,X1,X2,X3,X4,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A1)
!CDIR ON_ADB(A2)
!CDIR ON_ADB(A3)
!CDIR ON_ADB(A4)
!CDIR ON_ADB(X1)
!CDIR ON_ADB(X2)
!CDIR ON_ADB(X3)
!CDIR ON_ADB(X4)
!cdir nodep
!voption indep (A1,A2,A3,A4,X1,X2,X3,X4,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
            X3(in1)= A3(i)
            X4(in1)= A4(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine ordering_1x4_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x6_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,            &
     &           A4, A5, A6)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NP), A2(NP), A3(NP)
       real(kind=kreal), intent(in) :: A4(NP), A5(NP), A6(NP)
       real(kind=kreal), intent(inout) :: X1(NP), X2(NP), X3(NP)
       real(kind=kreal), intent(inout) :: X4(NP), X5(NP), X6(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A1,A2,A3,A4,A5,A6,X1,X2,X3,X4,X5,X6,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A1)
!CDIR ON_ADB(A2)
!CDIR ON_ADB(A3)
!CDIR ON_ADB(A4)
!CDIR ON_ADB(A5)
!CDIR ON_ADB(A6)
!CDIR ON_ADB(X1)
!CDIR ON_ADB(X2)
!CDIR ON_ADB(X3)
!CDIR ON_ADB(X4)
!CDIR ON_ADB(X5)
!CDIR ON_ADB(X6)
!cdir nodep
!voption indep (A1,A2,A3,A4,A5,A6,X1,X2,X3,X4,X5,X6,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
            X3(in1)= A3(i)
            X4(in1)= A4(i)
            X5(in1)= A5(i)
            X6(in1)= A6(i)
          end do
      enddo
!$omp end parallel do
!
       end subroutine ordering_1x6_by_old2new_L
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine ordering_int_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, iX, iA)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       integer(kind=kint), intent(in) :: iA(NP)
       integer(kind=kint), intent(inout) :: iX(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
!poption indep (A,X,OtoN_L,STACKmcG) tlocal (iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A)
!CDIR ON_ADB(X)
!cdir nodep
!voption indep (A,X,OtoN_L)
          do i= iS, iE
            in1   = OtoN_L(i)
            iX(in1)= iA(i)
          enddo
      enddo
!$omp end parallel do

       end subroutine ordering_int_by_old2new_L
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_o2nl_11
