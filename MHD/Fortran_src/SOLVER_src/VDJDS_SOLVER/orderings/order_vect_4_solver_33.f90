!
!      module order_vect_4_solver_33
!
!     Written by Kemorin
!
!       subroutine s_change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,   &
!     &           NtoO, B, X, W1, W2)
!       subroutine s_back_2_original_order_bx3(NP, NtoO, B, X, W1, W2)
!
      module order_vect_4_solver_33
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
       subroutine s_change_order_2_solve_bx3(NP, PEsmpTOT, STACKmcG,    &
     &           NtoO, B, X, W1, W2)
!
       integer(kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(3*NP)
       real(kind = kreal), intent(inout) :: W1(3*NP), W2(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP)
!
      integer (kind = kint) :: iS, iE, ip, i, in
!
!
!cdir parallel do private(iS,iE,i,in)
!$omp parallel do private(iS,iE,i,in)
!poption indep (W1,W2,B,NtoO,STACKmcG) tlocal (iS,iE,i,in)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W1,W2,B,NtoO)
        do i= iS, iE
          in= NtoO(i)
          W1(3*i-2)= B(3*in-2)
          W1(3*i-1)= B(3*in-1)
          W1(3*i  )= B(3*in  )
          W2(3*i-2)= X(3*in-2)
          W2(3*i-1)= X(3*in-1)
          W2(3*i  )= X(3*in  )
        enddo
      enddo
!$omp end parallel do

!cdir parallel do private(iS,iE,i)
!$omp parallel do private(iS,iE,i)
!poption indep (W1,W2,B,STACKmcG) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W1,W2,B)
        do i= iS, iE
          B(3*i-2)= W1(3*i-2)
          B(3*i-1)= W1(3*i-1)
          B(3*i  )= W1(3*i  )
          X(3*i-2)= W2(3*i-2)
          X(3*i-1)= W2(3*i-1)
          X(3*i  )= W2(3*i  )
        enddo
      enddo
!$omp end parallel do
!
!cdir parallel do
!$omp parallel do
!voption indep (W1,W2)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= 1, 3*NP
         W1(i)= 0.0d0
         W2(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine s_change_order_2_solve_bx3
!
!  ---------------------------------------------------------------------
!
       subroutine s_back_2_original_order_bx3(NP, NtoO, B, X, W1, W2)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: NtoO(NP)
       real(kind = kreal), intent(inout) :: B(3*NP)
       real(kind = kreal), intent(inout) :: W1(3*NP), W2(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP)
!
      integer (kind = kint) :: i, in
!
!
!$omp parallel do private(i,in) 
!voption indep (W1,W2,B,X,NtoO)
!cdir nodep
        do i= 1, NP
          in= NtoO(i)
          W1(3*in-2)= B(3*i-2)
          W1(3*in-1)= B(3*i-1)
          W1(3*in  )= B(3*i  )
          W2(3*in-2)= X(3*i-2)
          W2(3*in-1)= X(3*i-1)
          W2(3*in  )= X(3*i  )
        end do
!$omp end parallel do

!$omp parallel do private(i) 
!voption indep (W1,W2,B,X)
!OCL VECTOR, NOVREC
!cdir nodep
        do i= 1, NP
          B(3*i-2)= W1(3*i-2)
          B(3*i-1)= W1(3*i-1)
          B(3*i  )= W1(3*i  )
          X(3*i-2)= W2(3*i-2)
          X(3*i-1)= W2(3*i-1)
          X(3*i  )= W2(3*i  )
        enddo
!$omp end parallel do
!
!$omp parallel do
!voption indep (W1,W2)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= 1, 3*NP
         W1(i)= 0.0d0
         W2(i)= 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine s_back_2_original_order_bx3
!
!  ---------------------------------------------------------------------
!
      end module order_vect_4_solver_33
