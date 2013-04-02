!
!      module ordering_by_l2u_nn
!
!     Written by Kemorin
!
!       subroutine ordering_nx1_by_l2u(NP, NB, LtoU, X, A)
!       subroutine ordering_nx2_by_l2u(NP, NB, LtoU, X1, X2, A1, A2)
!       subroutine ordering_nx3_by_l2u(NP, NB, LtoU, X1, X2, X3,        &
!     &  A1, A2, A3)
!
      module ordering_by_l2u_nn
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
       subroutine ordering_nx1_by_l2u(NP, NB, LtoU, X, A)
!
       integer(kind = kint), intent(in) :: NP, NB
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP)
!
      integer (kind = kint) :: i, in1, ii, k1
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,ii)
!$omp parallel do private(in1,ii)
!poption indep (X,A,LtoU) tlocal (in1,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X,A,LtoU)
         do i= 1, NP
             ii  = NB*(  i - 1  ) + k1
             in1 = NB*(LtoU(i)-1) + k1
           X(in1)= A(ii)
         enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx1_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx2_by_l2u(NP, NB, LtoU, X1, X2, A1, A2)
!
       integer(kind = kint), intent(in) :: NP, NB
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind = kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
!
      integer (kind = kint) :: i, in1, ii, k1
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,ii)
!$omp parallel do private(in1,ii)
!poption indep (X1,X2,X3,A1,A2,LtoU) tlocal (in1,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,LtoU)
         do i= 1, NP
              ii  = NB*(  i - 1  ) + k1
              in1 = NB*(LtoU(i)-1) + k1
           X1(in1)= A1(ii)
           X2(in1)= A2(ii)
         enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx2_by_l2u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx3_by_l2u(NP, NB, LtoU, X1, X2, X3,         &
     &      A1, A2, A3)
!
       integer(kind = kint), intent(in) :: NP, NB
       integer(kind = kint), intent(in) :: LtoU(NP)
       real(kind = kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind = kreal), intent(in) :: A3(NB*NP)
       real(kind = kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
       real(kind = kreal), intent(inout) :: X3(NB*NP)
!
      integer (kind = kint) :: i, in1, ii, k1
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,ii)
!$omp parallel do private(in1,ii)
!poption indep (X1,X2,X3,A1,A2,A3,LtoU) tlocal (in1,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,A3,LtoU)
         do i= 1, NP
              ii  = NB*(  i - 1  ) + k1
              in1 = NB*(LtoU(i)-1) + k1
           X1(in1)= A1(ii)
           X2(in1)= A2(ii)
           X3(in1)= A3(ii)
         enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx3_by_l2u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_nn
