!
!      module order_vect_4_solver_nn
!
!     Written by Kemorin
!
!       subroutine s_change_order_2_solve_bxn(NP, NB, PEsmpTOT,         &
!     &           STACKmcG, NtoO, B, X, W1, W2)
!       subroutine s_back_2_original_order_bxn(NP, NB, NtoO, B, X,      &
!     &           W1, W2)
!
      module order_vect_4_solver_nn
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
       subroutine s_change_order_2_solve_bxn(NP, NB, PEsmpTOT,          &
     &           STACKmcG, NtoO, B, X, W1, W2)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(NB*NP)
       real(kind = kreal), intent(inout) :: W1(NB*NP), W2(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP)
!
      integer (kind = kint) :: iS, iE, ip, i, in, k1, ii
!
!
!$cdir parallel do private(iS,iE,i,in,ii.k1)
!$omp parallel do private(iS,iE,i,in,ii,k1)
!poption indep (W1,W2,X,B,NtoO,STACKmcG) tlocal (iS,iE,i,in,ii,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (W1,W2,X,B,NtoO)
          do i= iS, iE
            ii  = NB*(i-1) + k1
            in = NB*(NtoO(i)-1) + k1
            W1(ii)= B(in)
            W2(ii)= X(in)
          enddo
        end do
      enddo
!$omp end parallel do

!$cdir parallel do private(iS,iE,i,k1,ii)
!$omp parallel do private(iS,iE,i,k1,ii)
!poption indep (W1,W2,X,B,STACKmcG) tlocal (iS,iE,i,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (W1,W2,X,B)
          do i= iS, iE
            ii  = NB*(i-1) + k1
            B(ii)= W1(ii)
            X(ii)= W2(ii)
          enddo
        end do
      enddo
!$omp end parallel do
!
!cdir parallel do
!$omp parallel do
!voption indep (W1,W2)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= 1, NB*NP
         W1(i)= 0.0d0
         W2(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine s_change_order_2_solve_bxn
!
!  ---------------------------------------------------------------------
!
       subroutine s_back_2_original_order_bxn(NP, NB, NtoO, B, X,       &
     &           W1, W2)
!
       integer(kind = kint), intent(in) :: NP, NB
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(NB*NP)
       real(kind = kreal), intent(inout) :: W1(NB*NP), W2(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP)
!
      integer (kind = kint) :: i, in, k1, ii
!
!
!$omp parallel private(k1)
      do k1 = 1, NB
!$omp do private(i,in,ii) 
!cdir nodep noloopchg
        do i= 1, NP
            in = NB*(NtoO(i)-1) + k1
            ii = NB*(  i - 1  ) + k1
            W1(in)= B(ii)
            W2(in)= X(ii)
        enddo
!$omp end do
      enddo
!$omp end parallel

!$omp parallel private(k1)
      do k1 = 1, NB
!$cdir nodep noloopchg
!$omp do private(i,ii)
!voption indep (W1,W2,X,B)
        do i= 1, NP
            ii  = NB*(i-1) + k1
            B(ii)= W1(ii)
            X(ii)= W2(ii)
            W1(ii)= 0.0d0
            W2(ii)= 0.0d0
        enddo
!$omp end do
      enddo
!$omp end parallel
!
      end subroutine s_back_2_original_order_bxn
!
!  ---------------------------------------------------------------------
!
      end module order_vect_4_solver_nn
