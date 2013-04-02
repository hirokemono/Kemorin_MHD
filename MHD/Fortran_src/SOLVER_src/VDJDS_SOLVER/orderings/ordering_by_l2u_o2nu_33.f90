!
!      module ordering_by_l2u_o2nu_33
!
!     Written by Kemorin
!
!       subroutine ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!       subroutine ordering_3x2_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2,                    &
!     &          A_O2N_U1, A_O2N_U2, A_L2U1, A_L2U2)
!       subroutine ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,             &
!     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,  &
!     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
      module ordering_by_l2u_o2nu_33
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
       subroutine ordering_3x1_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U, X_L2U, A_O2N_U, A_L2U)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U(3*NP)
       real(kind = kreal), intent(in) :: A_O2N_U(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U(3*NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!cdir parallel do private(in1,in2)
!$omp parallel do private(in1,in2)
!CDIR ON_ADB(A_O2N_U)
!CDIR ON_ADB(A_L2U)
!CDIR ON_ADB(X_O2N_U)
!CDIR ON_ADB(X_L2U)
!cdir nodep
!voption indep (X_L2U,X_O2N_U,A_L2U,A_O2N_U,LtoU)
      do i= 1, NP
              in1          = OtoN_U(i)
              in2          = LtoU  (i)
          X_O2N_U(3*in1-2) = A_O2N_U(3*i-2)
          X_O2N_U(3*in1-1) = A_O2N_U(3*i-1)
          X_O2N_U(3*in1  ) = A_O2N_U(3*i  )
          X_L2U  (3*in2-2) = A_L2U  (3*i-2)
          X_L2U  (3*in2-1) = A_L2U  (3*i-1)
          X_L2U  (3*in2  ) = A_L2U  (3*i  )
      enddo
!$omp end parallel do

       end subroutine ordering_3x1_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x2_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U1, X_O2N_U2, X_L2U1, X_L2U2,                     &
     &          A_O2N_U1, A_O2N_U2, A_L2U1, A_L2U2)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(3*NP), A_O2N_U1(3*NP)
       real(kind = kreal), intent(in) :: A_L2U2(3*NP), A_O2N_U2(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U1(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U2(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U1(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U2(3*NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!cdir parallel do private(in1,in2)
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
!voption indep (X_O2N_U1,X_O2N_U2,X_L2U1,X_L2U2)
!voption indep (A_O2N_U1,A_O2N_U2,A_L2U1,A_L2U2,LtoU)
      do i= 1, NP
              in1          = OtoN_U(i)
              in2          = LtoU  (i)
          X_O2N_U1(3*in1-2) = A_O2N_U1(3*i-2)
          X_O2N_U1(3*in1-1) = A_O2N_U1(3*i-1)
          X_O2N_U1(3*in1  ) = A_O2N_U1(3*i  )
          X_O2N_U2(3*in1-2) = A_O2N_U2(3*i-2)
          X_O2N_U2(3*in1-1) = A_O2N_U2(3*i-1)
          X_O2N_U2(3*in1  ) = A_O2N_U2(3*i  )
          X_L2U1  (3*in2-2) = A_L2U1  (3*i-2)
          X_L2U1  (3*in2-1) = A_L2U1  (3*i-1)
          X_L2U1  (3*in2  ) = A_L2U1  (3*i  )
          X_L2U2  (3*in2-2) = A_L2U2  (3*i-2)
          X_L2U2  (3*in2-1) = A_L2U2  (3*i-1)
          X_L2U2  (3*in2  ) = A_L2U2  (3*i  )
      enddo
!$omp end parallel do

       end subroutine ordering_3x2_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_3x3_l2u_o2n_u(NP, OtoN_U, LtoU,              &
     &          X_O2N_U1, X_O2N_U2, X_O2N_U3, X_L2U1, X_L2U2, X_L2U3,   &
     &          A_O2N_U1, A_O2N_U2, A_O2N_U3, A_L2U1, A_L2U2, A_L2U3)
!
       integer(kind=kint), intent(in) :: NP
       integer(kind=kint), intent(in) :: LtoU(NP)
       integer(kind=kint), intent(in) :: OtoN_U(NP)
       real(kind = kreal), intent(in) :: A_L2U1(3*NP), A_O2N_U1(3*NP)
       real(kind = kreal), intent(in) :: A_L2U2(3*NP), A_O2N_U2(3*NP)
       real(kind = kreal), intent(in) :: A_L2U3(3*NP), A_O2N_U3(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U1(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U2(3*NP)
       real(kind = kreal), intent(inout) :: X_L2U3(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U1(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U2(3*NP)
       real(kind = kreal), intent(inout) :: X_O2N_U3(3*NP)
!
       integer (kind = kint) :: i, in1, in2
!
!
!cdir parallel do private(in1,in2)
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
!voption indep (X_O2N_U1,X_O2N_U2,X_O2N_U3,X_L2U1,X_L2U2,X_L2U3)
!voption indep (A_O2N_U1,A_O2N_U2,A_O2N_U3,A_L2U1,A_L2U2,A_L2U3,LtoU)
      do i= 1, NP
              in1          = OtoN_U(i)
              in2          = LtoU  (i)
          X_O2N_U1(3*in1-2) = A_O2N_U1(3*i-2)
          X_O2N_U1(3*in1-1) = A_O2N_U1(3*i-1)
          X_O2N_U1(3*in1  ) = A_O2N_U1(3*i  )
          X_O2N_U2(3*in1-2) = A_O2N_U2(3*i-2)
          X_O2N_U2(3*in1-1) = A_O2N_U2(3*i-1)
          X_O2N_U2(3*in1  ) = A_O2N_U2(3*i  )
          X_O2N_U3(3*in1-2) = A_O2N_U3(3*i-2)
          X_O2N_U3(3*in1-1) = A_O2N_U3(3*i-1)
          X_O2N_U3(3*in1  ) = A_O2N_U3(3*i  )
          X_L2U1  (3*in2-2) = A_L2U1  (3*i-2)
          X_L2U1  (3*in2-1) = A_L2U1  (3*i-1)
          X_L2U1  (3*in2  ) = A_L2U1  (3*i  )
          X_L2U2  (3*in2-2) = A_L2U2  (3*i-2)
          X_L2U2  (3*in2-1) = A_L2U2  (3*i-1)
          X_L2U2  (3*in2  ) = A_L2U2  (3*i  )
          X_L2U3  (3*in2-2) = A_L2U3  (3*i-2)
          X_L2U3  (3*in2-1) = A_L2U3  (3*i-1)
          X_L2U3  (3*in2  ) = A_L2U3  (3*i  )
      enddo
!$omp end parallel do

       end subroutine ordering_3x3_l2u_o2n_u
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_l2u_o2nu_33
