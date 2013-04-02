!incomplete_cholesky_crs_11.f90
!      module incomplete_cholesky_crs_11
!
!     Written by Kemorin
!
!C
!C***  Incomplete Cholesky
!C***
!C
!       subroutine i_cholesky_crs_11(NP, N, NPL, NPU,                   &
!     &        INL, INU, IAL, IAU, DD, AL, AU, S)
!       subroutine i_cholesky_crs_3x11(NP, N, NPL, NPU, INL, INU,       &
!     &           IAL, IAU, DD, AL, AU, S1, S2, S3)
!
      module incomplete_cholesky_crs_11
!
      use m_precision
!
       use ordering_by_o2nl_11
       use ordering_by_l2u_11
       use ordering_by_new2old_U_11
       use vector_calc_solver_11
       use forward_substitute_11
       use backward_substitute_11
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine i_cholesky_crs_11(NP, N, NPL, NPU,                    &
     &        INL, INU, IAL, IAU, DD, AL, AU, S)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: DD(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!       S(:) = WW(:,Z)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1
!C
!
!C== forward substitution
      do i= 1, N
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        W1 = S(i)
        do j= isL, ieL
          k = IAL(j)
          W1 =  W1 -  AL(j) * S(k)
        end do
        S(i) = W1 * DD(i)
      end do
!C
!C== backward elimination
      do i= N, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i  ) 
!
        W1 = 0.0d0
        do j= isU, ieU
          k = IAU(j)
          W1 = W1 + AU(j) * S(k)
        end do
        S(i) = S(i) - DD(i) * W1
      end do
!
      end subroutine i_cholesky_crs_11
!
!  ---------------------------------------------------------------------
!
       subroutine i_cholesky_crs_3x11(NP, N, NPL, NPU, INL, INU,        &
     &           IAL, IAU, DD, AL, AU, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: DD(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1, W2, W3
!
!
!C== forward substitution
      do i= 1, N
        isL= INL(i-1) + 1
        ieL= INL(i  ) 
        W1 = S1(i)
        W2 = S2(i)
        W3 = S3(i)
        do j= isL, ieL
          k = IAL(j)
          W1 =  W1 -  AL(j) * S1(k)
          W2 =  W2 -  AL(j) * S2(k)
          W3 =  W3 -  AL(j) * S3(k)
        end do
        S1(i) = W1 * DD(i)
        S2(i) = W2 * DD(i)
        S3(i) = W3 * DD(i)
      end do
!C
!C== backward elimination
      do i= N, 1, -1
        isU= INU(i-1) + 1
        ieU= INU(i  ) 
!
        W1 = 0.0d0
        W2 = 0.0d0
        W3 = 0.0d0
        do j= isU, ieU
          k = IAU(j)
          W1 = W1 + AU(j) * S1(k)
          W2 = W2 + AU(j) * S2(k)
          W3 = W3 + AU(j) * S3(k)
        end do
        S1(i) = S1(i) - DD(i) * W1
        S2(i) = S2(i) - DD(i) * W2
        S3(i) = S3(i) - DD(i) * W3
      end do
!
      end subroutine i_cholesky_crs_3x11
!
!  ---------------------------------------------------------------------
!
      end module incomplete_cholesky_crs_11
