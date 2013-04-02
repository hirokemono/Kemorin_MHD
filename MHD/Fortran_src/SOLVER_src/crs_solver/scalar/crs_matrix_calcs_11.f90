!
!      module crs_matrix_calcs_11
!
!     Written by Kemorin
!
!
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!       subroutine cal_crs_matvec_11                                    &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!       subroutine cal_crs_matvec_3x11                                  &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,      &
!     &            S1, S2, S3, X1, X2, X3)
!
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!       subroutine subtruct_crs_matvec_11 (NP, N, NPL, NPU,             &
!     &           INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine subtruct_crs_matvec_3x11                             &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,      &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
!
!       subroutine cal_gauss_zeidel_fw_11 (NP, N, NPL, NPU,             &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_gauss_zeidel_bw_11 (NP, N, NPL, NPU,             &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_jacob_fw_11 (NP, N, NPL, NPU,                    &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine cal_jacobi_bw_11 (NP, N, NPL, NPU,                   &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
      module crs_matrix_calcs_11
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
       subroutine cal_crs_matvec_11                                     &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: WVAL
!
!
      do j= 1, N
        WVAL= D(j) * X(j)

        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          WVAL= WVAL + AU(i) * X(k)
        end do

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
          WVAL= WVAL + AL(i) * X(k)
        end do
        S(j) = WVAL
      end do
!
      end subroutine cal_crs_matvec_11
!
!  ---------------------------------------------------------------------
!
       subroutine cal_crs_matvec_3x11                                   &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,       &
     &            S1, S2, S3, X1, X2, X3)
!
       integer(kind = kint), intent(in) :: NP, N, NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: X1(NP), X2(NP), X3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer(kind = kint) :: k, i, j, ist, ied
      real(kind = kreal) :: W1, W2, W3
!
!
      do j= 1, N
        W1 = D(j) * X1(j)
        W2 = D(j) * X2(j)
        W3 = D(j) * X3(j)

        ist= INU(j-1) + 1
        ied= INU(j  ) 
        do i= ist, ied
          k= IAU(i)
          W1= W1 + AU(i) * X1(k)
          W2= W2 + AU(i) * X2(k)
          W3= W3 + AU(i) * X3(k)
        end do

        ist= INL(j-1) + 1
        ied= INL(j  ) 
        do i= ist, ied
          k= IAL(i)
          W1= W1 + AL(i) * X1(k)
          W2= W2 + AL(i) * X2(k)
          W3= W3 + AL(i) * X3(k)
        end do
!
        S1(j) = W1
        S2(j) = W2
        S3(j) = W3
      end do
!
       end subroutine cal_crs_matvec_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_11 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: WVAL
!
!
      do j= 1, N
        WVAL= B(j) - D(j) * X(j)
        isU= INU(j-1) + 1
        ieU= INU(j  ) 

        do i= isU, ieU
          k= IAU(i)
          WVAL= WVAL - AU(i) * X(k)
        enddo

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
          WVAL= WVAL - AL(i) * X(k)
        enddo
        S(j)= WVAL
      enddo
!
      end subroutine subtruct_crs_matvec_11
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_3x11                              &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,       &
     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
       integer(kind = kint), intent(in) :: NP, N, NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
       real(kind = kreal), intent(in) :: B1(NP), B2(NP), B3(NP)
       real(kind = kreal), intent(in) :: X1(NP), X2(NP), X3(NP)
!
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer(kind = kint) :: k, i, j, ist, ied
      real(kind = kreal) :: W1, W2, W3
!
!
      do j= 1, N
        W1 = B1(j) - D(j) * X1(j)
        W2 = B2(j) - D(j) * X2(j)
        W3 = B3(j) - D(j) * X3(j)

        ist= INU(j-1) + 1
        ied= INU(j  ) 
        do i= ist, ied
          k= IAU(i)
          W1= W1 - AU(i) * X1(k)
          W2= W2 - AU(i) * X2(k)
          W3= W3 - AU(i) * X3(k)
        end do

        ist= INL(j-1) + 1
        ied= INL(j  ) 
        do i= ist, ied
          k= IAL(i)
          W1= W1 - AL(i) * X1(k)
          W2= W2 - AL(i) * X2(k)
          W3= W3 - AL(i) * X3(k)
        end do
!
        S1(j) = W1
        S2(j) = W2
        S3(j) = W3
      end do

       end subroutine subtruct_crs_matvec_3x11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_fw_11 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v
!
!
       DO i = 1, N
          v = B(i)
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v = v - AL(j) * S(k)
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v = v - AU(j) * S(k)
          END DO

          S(i) = v / D(i)
       END DO
!
      end subroutine cal_gauss_zeidel_fw_11
!
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_bw_11 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v
!
!
       DO i = N, 1, -1
          v = B(i)

          isL = INL(i - 1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v = v - AL(j) * S(k)
          END DO
          
          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v = v - AU(j) * S(k)
          END DO

          S(i) = v / D(i)
       END DO
!
      end subroutine cal_gauss_zeidel_bw_11
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacob_fw_11 (NP, N, NPL, NPU,                     &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v
!
!
       DO i = 1, N
          v = B(i)
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v = v - AL(j) * X(k)
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v = v - AU(j) * X(k)
          END DO

          S(i) = v / D(i)
       END DO
!
      end subroutine cal_jacob_fw_11
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacobi_bw_11 (NP, N, NPL, NPU,                    &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NP)
       real(kind = kreal), intent(in) :: X(NP)
       real(kind = kreal), intent(in) :: D(NP)
       real(kind = kreal), intent(in) :: AL(NPL)
       real(kind = kreal), intent(in) :: AU(NPU)
!
       real(kind = kreal), intent(inout) :: S(NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v
!
!
       DO i = N, 1, -1
          v = B(i)

          isL = INL(i - 1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v = v - AL(j) * X(k)
          END DO
          
          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v = v - AU(j) * X(k)
          END DO

          S(i) = v / D(i)
       END DO
!
      end subroutine cal_jacobi_bw_11
!
!  ---------------------------------------------------------------------
!
      end module crs_matrix_calcs_11
