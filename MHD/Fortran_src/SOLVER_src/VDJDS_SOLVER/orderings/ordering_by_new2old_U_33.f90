!
!      module ordering_by_new2old_U_33
!
!     Written by Kemorin
!
!       subroutine ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,    &
!     &           NtoO_U, X, A)
!       subroutine ordering_3x2_by_new2old_U(NP, PEsmpTOT, STACKmcG,    &
!     &           NtoO_U, X1, X2, A1, A2)
!       subroutine ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,    &
!     &           NtoO_U, X1, X2, X3, A1, A2, A3)
!
      module ordering_by_new2old_U_33
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
       subroutine ordering_3x1_by_new2old_U(NP, PEsmpTOT, STACKmcG,     &
     &           NtoO_U, X, A)
!
       integer(kind = kint), intent(in)  :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
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
!cdir nodep
!voption indep (X,A,NtoO_U)
        do i= iS, iE
              in1      = NtoO_U(i)
          X(3*in1-2)= A(3*i-2)
          X(3*in1-1)= A(3*i-1)
          X(3*in1  )= A(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x1_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x2_by_new2old_U(NP, PEsmpTOT, STACKmcG,     &
     &           NtoO_U, X1, X2, A1, A2)
!
       integer(kind = kint), intent(in)  :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
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
!cdir nodep
!voption indep (X,A,NtoO_U)
        do i= iS, iE
              in1    = NtoO_U(i)
          X1(3*in1-2)= A1(3*i-2)
          X1(3*in1-1)= A1(3*i-1)
          X1(3*in1  )= A1(3*i  )
          X2(3*in1-2)= A2(3*i-2)
          X2(3*in1-1)= A2(3*i-1)
          X2(3*in1  )= A2(3*i  )
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_3x2_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x3_by_new2old_U(NP, PEsmpTOT, STACKmcG,     &
     &           NtoO_U, X1, X2, X3, A1, A2, A3)
!
       integer(kind = kint), intent(in)  :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A1(3*NP), A2(3*NP)
       real(kind = kreal), intent(in) :: A3(3*NP)
       real(kind = kreal), intent(inout) :: X1(3*NP), X2(3*NP)
       real(kind = kreal), intent(inout) :: X3(3*NP)
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
!cdir nodep
!voption indep (X,A,NtoO_U)
        do i= iS, iE
              in1    = NtoO_U(i)
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

       end subroutine ordering_3x3_by_new2old_U
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_new2old_U_33
