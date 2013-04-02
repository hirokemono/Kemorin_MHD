!
!      module order_vect_4_solver_11
!
!     Written by Kemorin
!
!       subroutine s_change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,   &
!     &           NtoO, B, X, W1, W2)
!       subroutine s_back_2_original_order_bx1(NP, NtoO, B, X, W1, W2)
!
!       subroutine change_order_2_solve_int(NP, PEsmpTOT, STACKmcG,     &
!     &           NtoO, iX, iW)
!       subroutine back_2_original_order_int(NP, NtoO, iX, iW)
!
      module order_vect_4_solver_11
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
       subroutine s_change_order_2_solve_bx1(NP, PEsmpTOT, STACKmcG,    &
     &           NtoO, B, X, W1, W2)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(NP)
       real(kind = kreal), intent(inout) :: W1(NP), W2(NP)
       real(kind = kreal), intent(inout) :: X(NP)
!
      integer (kind = kint) :: iS, iE, ip, i, in
!
!
!cdir parallel do private(iS,iE,i,in)
!$omp parallel do private(iS,iE,i,in)
!poption indep (W1,W2,X,B,NtoO,STACKmcG) tlocal (iS,iE,i,in)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep noloopchg
!voption indep (W1,W2,X,B,NtoO)
          do i= iS, iE
            in = NtoO(i)
            W1(i)= B(in)
            W2(i)= X(in)
          enddo
      enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (W1,W2,X,B,STACKmcG) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep noloopchg
!voption indep (W1,W2,X,B)
          do i= iS, iE
            B(i)= W1(i)
            X(i)= W2(i)
          enddo
      enddo
!$omp end parallel do
!
!cdir parallel do
!$omp parallel do
!voption indep (W1,W2)
!cdir nodep
       do i= 1, NP
         W1(i)= 0.0d0
         W2(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine s_change_order_2_solve_bx1
!
!  ---------------------------------------------------------------------
!
       subroutine s_back_2_original_order_bx1(NP, NtoO, B, X, W1, W2)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(NP)
       real(kind = kreal), intent(inout) :: W1(NP), W2(NP)
       real(kind = kreal), intent(inout) :: X(NP)
!
      integer (kind = kint) :: i, in
!
!
!$omp parallel do private(i,in) 
!voption indep (W1,W2,B,X,NtoO)
!cdir nodep noloopchg
          do i= 1, NP
            in = NtoO(i)
            W1(in) = B(i)
            W2(in) = X(i)
          enddo
!$omp end parallel do

!$omp parallel do private(i)
!cdir nodep noloopchg
!voption indep (W1,W2,X,B)
          do i= 1, NP
            B(i)= W1(i)
            X(i)= W2(i)
          enddo
!$omp end parallel do
!
!$omp parallel do
!voption indep (W1,W2)
!cdir nodep
       do i= 1, NP
         W1(i)= 0.0d0
         W2(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine s_back_2_original_order_bx1
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine change_order_2_solve_int(NP, PEsmpTOT, STACKmcG,      &
     &           NtoO, iX, iW)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       integer(kind = kint), intent(inout) :: iW(NP)
       integer(kind = kint), intent(inout) :: iX(NP)
!
      integer (kind = kint) :: iS, iE, ip, i, in
!
!
!cdir parallel do private(iS,iE,i,in)
!$omp parallel do private(iS,iE,i,in)
!poption indep (iW,iX,NtoO,STACKmcG) tlocal (iS,iE,i,in)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep noloopchg
!voption indep (iW,iX,NtoO)
          do i= iS, iE
            in = NtoO(i)
            iW(i)= iX(in)
          enddo
      enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (iW,iX,STACKmcG) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!cdir nodep noloopchg
!voption indep (iW,iX)
          do i= iS, iE
            iX(i)= iW(i)
          enddo
      enddo
!$omp end parallel do
!
!cdir parallel do
!$omp parallel do
!poption indep (W)
!voption indep (W)
!cdir nodep
       do i= 1, NP
         iW(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine change_order_2_solve_int
!
!  ---------------------------------------------------------------------
!
       subroutine back_2_original_order_int(NP, NtoO, iX, iW)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: NtoO(NP)
       integer(kind = kint), intent(inout) :: iW(NP)
       integer(kind = kint), intent(inout) :: iX(NP)
!
      integer (kind = kint) :: i, in
!
!
!$omp parallel do private(i,in) 
!voption indep (iW,iX,NtoO)
!cdir nodep noloopchg
       do i= 1, NP
            in = NtoO(i)
            iW(in)= iX(i)
        enddo
!$omp end parallel do

!$omp parallel do private(i)
!cdir nodep noloopchg
!voption indep (iW,iX)
       do i= 1, NP
            iX(i)= iW(i)
       enddo
!$omp end parallel do
!
!$omp parallel do
!cdir nodep
       do i= 1, NP
         iW(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine back_2_original_order_int
!
!  ---------------------------------------------------------------------
!
      end module order_vect_4_solver_11
