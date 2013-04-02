!
!      module ordering_by_l2u_11
!
!     Written by Kemorin
!
!       subroutine ordering_1x1_by_l2u(NP, LtoU, X, A)
!       subroutine ordering_1x2_by_l2u(NP, LtoU, X1, X2, A1, A2)
!       subroutine ordering_1x3_by_l2u(NP, LtoU, X1, X2, X3, A1, A2, A3)
!
!       subroutine ordering_int_by_l2u(NP, LtoU, iX, iA)
!
      module ordering_by_l2u_11
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
       subroutine ordering_1x1_by_l2u(NP, LtoU, X, A)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A(NP)
       real(kind = kreal), intent(inout) :: X(NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (X,A,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X,A,LtoU)
        do i= 1, NP
            in1 = LtoU(i)
          X(in1)= A(i)
        enddo
!$omp end parallel do

       end subroutine ordering_1x1_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x2_by_l2u(NP, LtoU, X1, X2, A1, A2)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(NP), A2(NP)
       real(kind = kreal), intent(inout) :: X1(NP), X2(NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (X1,X2,X3,A1,A2,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,LtoU)
         do i= 1, NP
              in1 = LtoU(i)
           X1(in1)= A1(i)
           X2(in1)= A2(i)
         enddo
!$omp end parallel do

       end subroutine ordering_1x2_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x3_by_l2u(NP, LtoU, X1, X2, X3, A1, A2, A3)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(NP), A2(NP)
       real(kind = kreal), intent(in) :: A3(NP)
       real(kind = kreal), intent(inout) :: X1(NP), X2(NP)
       real(kind = kreal), intent(inout) :: X3(NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (X1,X2,X3,A1,A2,A3,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,A3,LtoU)
         do i= 1, NP
              in1 = LtoU(i)
           X1(in1)= A1(i)
           X2(in1)= A2(i)
           X3(in1)= A3(i)
         enddo
!$omp end parallel do

       end subroutine ordering_1x3_by_l2u
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine ordering_int_by_l2u(NP, LtoU, iX, iA)
!
       integer(kind = kint), intent(in) :: NP
       integer(kind = kint), intent(in) :: LtoU(NP)
       integer(kind = kint), intent(in) :: iA(NP)
       integer(kind = kint), intent(inout) :: iX(NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (X,A,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X,A,LtoU)
        do i= 1, NP
            in1 = LtoU(i)
          iX(in1)= iA(i)
        enddo
!$omp end parallel do

       end subroutine ordering_int_by_l2u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_11
