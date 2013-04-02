!
!      module ordering_by_o2nl_33
!
!     Written by Kemorin
!
!       subroutine ordering_3x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X, A)
!       subroutine ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, A1, A2)
!       subroutine ordering_3x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!       subroutine ordering_3x4_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!       subroutine ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG,    &
!     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,           &
!     &           A4, A5, A6)
!
      module ordering_by_o2nl_33
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
       subroutine ordering_3x1_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X, A)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
       real(kind = kreal), intent(in) :: A(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A)
!CDIR ON_ADB(X)
!cdir nodep
        do i= iS, iE
           in1      = OtoN_L(i)
          X(3*in1-2)= A(3*i-2)
          X(3*in1-1)= A(3*i-1)
          X(3*in1  )= A(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x1_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x2_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, A1, A2)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
       real(kind = kreal), intent(in) :: A1(3*NP), A2(3*NP)
       real(kind = kreal), intent(inout) :: X1(3*NP), X2(3*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(A1)
!CDIR ON_ADB(A2)
!CDIR ON_ADB(X1)
!CDIR ON_ADB(X2)
!cdir nodep
        do i= iS, iE
            in1      = OtoN_L(i)
          X1(3*in1-2)= A1(3*i-2)
          X1(3*in1-1)= A1(3*i-1)
          X1(3*in1  )= A1(3*i  )
          X2(3*in1-2)= A2(3*i-2)
          X2(3*in1-1)= A2(3*i-1)
          X2(3*in1  )= A2(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x2_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x3_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
       real(kind=kreal), intent(in) :: A1(3*NP), A2(3*NP), A3(3*NP)
       real(kind=kreal), intent(inout) :: X1(3*NP), X2(3*NP), X3(3*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
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
        do i= iS, iE
            in1      = OtoN_L(i)
          X1(3*in1-2)= A1(3*i-2)
          X1(3*in1-1)= A1(3*i-1)
          X1(3*in1  )= A1(3*i  )
          X2(3*in1-2)= A2(3*i-2)
          X2(3*in1-1)= A2(3*i-1)
          X2(3*in1  )= A2(3*i  )
          X3(3*in1-2)= A3(3*i-2)
          X3(3*in1-1)= A3(3*i-1)
          X3(3*in1  )= A3(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x3_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x4_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
       real(kind=kreal), intent(in) :: A1(3*NP), A2(3*NP)
       real(kind=kreal), intent(in) :: A3(3*NP), A4(3*NP)
       real(kind=kreal), intent(inout) :: X1(3*NP), X2(3*NP)
       real(kind=kreal), intent(inout) :: X3(3*NP), X4(3*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
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
        do i= iS, iE
            in1      = OtoN_L(i)
          X1(3*in1-2)= A1(3*i-2)
          X1(3*in1-1)= A1(3*i-1)
          X1(3*in1  )= A1(3*i  )
          X2(3*in1-2)= A2(3*i-2)
          X2(3*in1-1)= A2(3*i-1)
          X2(3*in1  )= A2(3*i  )
          X3(3*in1-2)= A3(3*i-2)
          X3(3*in1-1)= A3(3*i-1)
          X3(3*in1  )= A3(3*i  )
          X4(3*in1-2)= A4(3*i-2)
          X4(3*in1-1)= A4(3*i-1)
          X4(3*in1  )= A4(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x4_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x6_by_old2new_L(NP, PEsmpTOT, STACKmcG,     &
     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,            &
     &           A4, A5, A6)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
       real(kind=kreal), intent(in) :: A1(3*NP), A2(3*NP), A3(3*NP)
       real(kind=kreal), intent(in) :: A4(3*NP), A5(3*NP), A6(3*NP)
       real(kind=kreal), intent(inout) :: X1(3*NP), X2(3*NP), X3(3*NP)
       real(kind=kreal), intent(inout) :: X4(3*NP), X5(3*NP), X6(3*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
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
        do i= iS, iE
            in1      = OtoN_L(i)
          X1(3*in1-2)= A1(3*i-2)
          X1(3*in1-1)= A1(3*i-1)
          X1(3*in1  )= A1(3*i  )
          X2(3*in1-2)= A2(3*i-2)
          X2(3*in1-1)= A2(3*i-1)
          X2(3*in1  )= A2(3*i  )
          X3(3*in1-2)= A3(3*i-2)
          X3(3*in1-1)= A3(3*i-1)
          X3(3*in1  )= A3(3*i  )
          X4(3*in1-2)= A4(3*i-2)
          X4(3*in1-1)= A4(3*i-1)
          X4(3*in1  )= A4(3*i  )
          X5(3*in1-2)= A5(3*i-2)
          X5(3*in1-1)= A5(3*i-1)
          X5(3*in1  )= A5(3*i  )
          X6(3*in1-2)= A6(3*i-2)
          X6(3*in1-1)= A6(3*i-1)
          X6(3*in1  )= A6(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x6_by_old2new_L
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_o2nl_33
