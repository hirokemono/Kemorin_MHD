!
!      module ordering_by_l2u_33
!
!     Written by Kemorin
!
!       subroutine ordering_3x1_by_l2u(NP, LtoU, X, A)
!       subroutine ordering_3x2_by_l2u(NP, LtoU, X1, X2, A1, A2)
!       subroutine ordering_3x3_by_l2u(NP, LtoU, X1, X2, X3, A1, A2, A3)
!
      module ordering_by_l2u_33
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
       subroutine ordering_3x1_by_l2u(NP, LtoU, X, A)
!
       integer (kind = kint) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A(3*NP)
       real(kind = kreal), intent(inout) :: X(3*NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (W,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W,LtoU)
      do i= 1, NP
            in1     = LtoU(i)
        X(3*in1-2)= A(3*i-2)
        X(3*in1-1)= A(3*i-1)
        X(3*in1  )= A(3*i  )
      enddo
!$omp end parallel do
!
       end subroutine ordering_3x1_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x2_by_l2u(NP, LtoU, X1, X2, A1, A2)
!
       integer (kind = kint) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(3*NP), A2(3*NP)
       real(kind = kreal), intent(inout) :: X1(3*NP), X2(3*NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (W,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W,LtoU)
      do i= 1, NP
            in1     = LtoU(i)
        X1(3*in1-2)= A1(3*i-2)
        X1(3*in1-1)= A1(3*i-1)
        X1(3*in1  )= A1(3*i  )
        X2(3*in1-2)= A2(3*i-2)
        X2(3*in1-1)= A2(3*i-1)
        X2(3*in1  )= A2(3*i  )
      enddo
!$omp end parallel do
!
       end subroutine ordering_3x2_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x3_by_l2u(NP, LtoU, X1, X2, X3, A1, A2, A3)
!
       integer (kind = kint) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(3*NP), A2(3*NP)
       real(kind = kreal), intent(in) :: A3(3*NP)
       real(kind = kreal), intent(inout) :: X1(3*NP), X2(3*NP)
       real(kind = kreal), intent(inout) :: X3(3*NP)
!
      integer (kind = kint) :: i, in1
!
!
!cdir parallel do private(in1)
!$omp parallel do private(in1)
!poption indep (W,LtoU) tlocal (in1)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W,LtoU)
      do i= 1, NP
            in1     = LtoU(i)
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
!$omp end parallel do
!
       end subroutine ordering_3x3_by_l2u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_33
