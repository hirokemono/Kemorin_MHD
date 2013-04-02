!
!      module ordering_by_l2u_o2nu_11
!
!     Written by Kemorin
!
!       subroutine ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!       subroutine ordering_1x2_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2,                    &
!     &          A_O2N_U1, A_O2N_U2, A_L2U1, A_L2U2)
!       subroutine ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,  &
!     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
!       subroutine ordering_int_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          iX_O2N_U, iX_L2U, iA_O2N_U, iA_L2U)
!
      module ordering_by_l2u_o2nu_11
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
       subroutine ordering_1x1_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U(NP)
       real(kind = kreal), intent(in) :: A_O2N_U(NP)
       real(kind = kreal), intent(inout) :: X_L2U(NP)
       real(kind = kreal), intent(inout) :: X_O2N_U(NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!$omp parallel do private(in1,in2)
!CDIR ON_ADB(A_O2N_U)
!CDIR ON_ADB(A_L2U)
!CDIR ON_ADB(X_O2N_U)
!CDIR ON_ADB(X_L2U)
!cdir nodep
        do i= 1, NP
              in1      = OtoN_U(i)
              in2      = LtoU  (i)
          X_O2N_U(in1) = A_O2N_U(i)
          X_L2U  (in2) = A_L2U  (i)
        enddo
!$omp end parallel do

       end subroutine ordering_1x1_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x2_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2,                     &
     &          A_O2N_U1, A_O2N_U2, A_L2U1, A_L2U2)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(NP), A_O2N_U1(NP)
       real(kind = kreal), intent(in) :: A_L2U2(NP), A_O2N_U2(NP)
       real(kind = kreal), intent(inout) :: X_L2U1(NP), X_O2N_U1(NP)
       real(kind = kreal), intent(inout) :: X_L2U2(NP), X_O2N_U2(NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!$omp parallel do private(in1,in2)
!CDIR ON_ADB(A_O2N_U1)
!CDIR ON_ADB(A_O2N_U2)
!CDIR ON_ADB(A_L2U1)
!CDIR ON_ADB(A_L2U2)
!CDIR ON_ADB(X_O2N_U1)
!CDIR ON_ADB(X_O2N_U2)
!CDIR ON_ADB(X_L2U1)
!CDIR ON_ADB(X_L2U2)
!cdir nodep
        do i= 1, NP
              in1      = OtoN_U(i)
              in2      = LtoU  (i)
          X_O2N_U1(in1) = A_O2N_U1(i)
          X_O2N_U2(in1) = A_O2N_U2(i)
          X_L2U1  (in2) = A_L2U1  (i)
          X_L2U2  (in2) = A_L2U2  (i)
        enddo
!$omp end parallel do

       end subroutine ordering_1x2_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_1x3_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,   &
     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(NP), A_O2N_U1(NP)
       real(kind = kreal), intent(in) :: A_L2U2(NP), A_O2N_U2(NP)
       real(kind = kreal), intent(in) :: A_L2U3(NP), A_O2N_U3(NP)
       real(kind = kreal), intent(inout) :: X_L2U1(NP), X_O2N_U1(NP)
       real(kind = kreal), intent(inout) :: X_L2U2(NP), X_O2N_U2(NP)
       real(kind = kreal), intent(inout) :: X_L2U3(NP), X_O2N_U3(NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!$omp parallel do private(in1,in2)
!CDIR ON_ADB(A_O2N_U1)
!CDIR ON_ADB(A_O2N_U2)
!CDIR ON_ADB(A_O2N_U3)
!CDIR ON_ADB(A_L2U1)
!CDIR ON_ADB(A_L2U2)
!CDIR ON_ADB(A_L2U3)
!CDIR ON_ADB(X_O2N_U1)
!CDIR ON_ADB(X_O2N_U2)
!CDIR ON_ADB(X_O2N_U3)
!CDIR ON_ADB(X_L2U1)
!CDIR ON_ADB(X_L2U2)
!CDIR ON_ADB(X_L2U3)
!cdir nodep
        do i= 1, NP
              in1      = OtoN_U(i)
              in2      = LtoU  (i)
          X_O2N_U1(in1) = A_O2N_U1(i)
          X_O2N_U2(in1) = A_O2N_U2(i)
          X_O2N_U3(in1) = A_O2N_U3(i)
          X_L2U1  (in2) = A_L2U1  (i)
          X_L2U2  (in2) = A_L2U2  (i)
          X_L2U3  (in2) = A_L2U3  (i)
        enddo
!$omp end parallel do

       end subroutine ordering_1x3_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine ordering_int_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          iX_O2N_U, iX_L2U, iA_O2N_U, iA_L2U)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       integer(kind = kint), intent(in) :: iA_L2U(NP)
       integer(kind = kint), intent(in) :: iA_O2N_U(NP)
       integer(kind = kint), intent(inout) :: iX_L2U(NP)
       integer(kind = kint), intent(inout) :: iX_O2N_U(NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!$omp parallel do private(in1,in2)
!CDIR ON_ADB(A_O2N_U)
!CDIR ON_ADB(A_L2U)
!CDIR ON_ADB(X_O2N_U)
!CDIR ON_ADB(X_L2U)
!cdir nodep
        do i= 1, NP
              in1      = OtoN_U(i)
              in2      = LtoU  (i)
          iX_O2N_U(in1) = iA_O2N_U(i)
          iX_L2U  (in2) = iA_L2U  (i)
        enddo
!$omp end parallel do

       end subroutine ordering_int_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_o2nu_11
