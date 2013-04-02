!
!      module ordering_by_new2old_U_11
!
!     Written by Kemorin
!
!       subroutine ordering_1x1_by_new2old_U(NP, PEsmpTOT,              &
!     &           STACKmcG, NtoO_U, X, A)
!       subroutine ordering_1x2_by_new2old_U(NP, PEsmpTOT,              &
!     &           STACKmcG, NtoO_U, X1, X2, A1, A2)
!       subroutine ordering_1x3_by_new2old_U(NP, PEsmpTOT,              &
!     &           STACKmcG, NtoO_U, X1, X2, X3, A1, A2, A3)
!
!       subroutine ordering_int_by_new2old_U(NP, PEsmpTOT,              &
!     &           STACKmcG, NtoO_U, iX, iA)
!
      module ordering_by_new2old_U_11
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
       subroutine ordering_1x1_by_new2old_U(NP, PEsmpTOT,               &
     &           STACKmcG, NtoO_U, X, A)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A(NP)
       real(kind = kreal), intent(inout) :: X(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!CDIR ON_ADB(A)
          do i= iS, iE
              in1 = NtoO_U(i)
            X(in1)= A(i)
          end do
      enddo
!$omp end parallel do

       end subroutine ordering_1x1_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x2_by_new2old_U(NP, PEsmpTOT,               &
     &           STACKmcG, NtoO_U, X1, X2, A1, A2)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A1(NP), A2(NP)
       real(kind = kreal), intent(inout) :: X1(NP), X2(NP)
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
          do i= iS, iE
              in1 = NtoO_U(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
          end do
      end do
!$omp end parallel do

       end subroutine ordering_1x2_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x3_by_new2old_U(NP, PEsmpTOT,               &
     &           STACKmcG, NtoO_U, X1, X2, X3, A1, A2, A3)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A1(NP), A2(NP)
       real(kind = kreal), intent(in) :: A3(NP)
       real(kind = kreal), intent(inout) :: X1(NP), X2(NP)
       real(kind = kreal), intent(inout) :: X3(NP)
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
          do i= iS, iE
              in1 = NtoO_U(i)
            X1(in1)= A1(i)
            X2(in1)= A2(i)
            X3(in1)= A3(i)
          end do
      end do
!$omp end parallel do

       end subroutine ordering_1x3_by_new2old_U
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine ordering_int_by_new2old_U(NP, PEsmpTOT,               &
     &           STACKmcG, NtoO_U, iX, iA)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       integer(kind = kint), intent(in) :: iA(NP)
       integer(kind = kint), intent(inout) :: iX(NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1
!
!
!$omp parallel do private(iS,iE,i,in1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep
!CDIR ON_ADB(A)
          do i= iS, iE
              in1 = NtoO_U(i)
            iX(in1)= iA(i)
          end do
      end do
!$omp end parallel do

       end subroutine ordering_int_by_new2old_U
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_new2old_U_11
