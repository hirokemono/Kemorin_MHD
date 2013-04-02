!
!      module ordering_by_l2u_o2nu_nn
!
!     Written by Kemorin
!
!       subroutine ordering_nx1_l2u_o2n_u(NP, NB, OtoN_U, LtoU,         &
!     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!       subroutine ordering_nx2_l2u_o2n_u(NP, NB, OtoN_U, LtoU,         &
!     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2, A_O2N_U1,          &
!     &          A_O2N_U2, A_L2U1, A_L2U2)
!       subroutine ordering_nx3_l2u_o2n_u(NP, NB, OtoN_U, LtoU,         &
!     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,  &
!     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
      module ordering_by_l2u_o2nu_nn
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
       subroutine ordering_nx1_l2u_o2n_u(NP, NB, OtoN_U, LtoU,          &
     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!
       integer(kind=kint), intent(in) :: NP, NB
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U(NB*NP)
       real(kind = kreal), intent(in) :: A_O2N_U(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U(NB*NP)
!
       integer (kind = kint) :: i, k1, in1, in2, ii
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,in2,ii)
!$omp parallel do private(in1,in2,ii)
!poption indep (X_O2N_U,X_L2U,A_O2N_U,A_L2U,LtoU) tlocal (in1,in2,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X_O2N_U,X_L2U,A_O2N_U,A_L2U,LtoU)
        do i= 1, NP
              ii       = NB*(i-1) + k1
              in1      = NB*(OtoN_U(i)-1) + k1
              in2      = NB*(LtoU  (i)-1) + k1
          X_O2N_U(in1) = A_O2N_U(ii)
          X_L2U  (in2) = A_L2U  (ii)
        enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx1_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx2_l2u_o2n_u(NP, NB, OtoN_U, LtoU,          &
     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2, A_O2N_U1,           &
     &          A_O2N_U2, A_L2U1, A_L2U2)
!
       integer(kind=kint), intent(in) :: NP, NB
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(NB*NP), A_O2N_U1(NB*NP)
       real(kind = kreal), intent(in) :: A_L2U2(NB*NP), A_O2N_U2(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U1(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U2(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U1(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U2(NB*NP)
!
       integer (kind = kint) :: i, k1, in1, in2, ii
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,in2,ii)
!$omp parallel do private(in1,in2,ii)
!poption indep (X_O2N_U1,X_O2N_U2,X_L2U1,X_L2U2)
!poption indep (A_O2N_U1,A_O2N_U2,A_L2U1,A_L2U2,LtoU)
!poption tlocal (in1,in2,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X_O2N_U1,X_O2N_U2,X_L2U1,X_L2U2)
!voption indep (A_O2N_U1,A_O2N_U2,A_L2U1,A_L2U2,LtoU)
        do i= 1, NP
              ii       = NB*(i-1) + k1
              in1      = NB*(OtoN_U(i)-1) + k1
              in2      = NB*(LtoU  (i)-1) + k1
          X_O2N_U1(in1) = A_O2N_U1(ii)
          X_O2N_U2(in1) = A_O2N_U2(ii)
          X_L2U1  (in2) = A_L2U1  (ii)
          X_L2U2  (in2) = A_L2U2  (ii)
        enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx2_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx3_l2u_o2n_u(NP, NB, OtoN_U, LtoU,          &
     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,   &
     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
       integer(kind=kint), intent(in) :: NP, NB
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(NB*NP), A_O2N_U1(NB*NP)
       real(kind = kreal), intent(in) :: A_L2U2(NB*NP), A_O2N_U2(NB*NP)
       real(kind = kreal), intent(in) :: A_L2U3(NB*NP), A_O2N_U3(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U1(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U2(NB*NP)
       real(kind = kreal), intent(inout) :: X_L2U3(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U1(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U2(NB*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U3(NB*NP)
!
       integer (kind = kint) :: i, k1, in1, in2, ii
!
!
       do k1 = 1, NB
!cdir parallel do private(in1,in2,ii)
!$omp parallel do private(in1,in2,ii)
!poption indep (X_O2N_U1,X_O2N_U2,X_O2N_U3,X_L2U1,X_L2U2,X_L2U3)
!poption indep (A_O2N_U1,A_O2N_U2,A_O2N_U3,A_L2U1,A_L2U2,A_L2U3,LtoU)
!poption tlocal (in1,in2,ii)
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X_O2N_U1,X_O2N_U2,X_O2N_U3,X_L2U1,X_L2U2,X_L2U3)
!voption indep (A_O2N_U1,A_O2N_U2,A_O2N_U3,A_L2U1,A_L2U2,A_L2U3,LtoU)
        do i= 1, NP
              ii       = NB*(i-1) + k1
              in1      = NB*(OtoN_U(i)-1) + k1
              in2      = NB*(LtoU  (i)-1) + k1
          X_O2N_U1(in1) = A_O2N_U1(ii)
          X_O2N_U2(in1) = A_O2N_U2(ii)
          X_O2N_U3(in1) = A_O2N_U3(ii)
          X_L2U1  (in2) = A_L2U1  (ii)
          X_L2U2  (in2) = A_L2U2  (ii)
          X_L2U3  (in2) = A_L2U3  (ii)
        enddo
!$omp end parallel do
       end do

       end subroutine ordering_nx3_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_o2nu_nn
