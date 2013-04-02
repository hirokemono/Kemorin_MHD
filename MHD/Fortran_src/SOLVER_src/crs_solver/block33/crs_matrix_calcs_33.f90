!
!      module crs_matrix_calcs_33
!
!     Written by Kemorin
!
!
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!       subroutine cal_crs_matvec_33                                    &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!       subroutine cal_crs_matvec_3x33                                  &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,      &
!     &            S1, S2, S3, X1, X2, X3)
!
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!       subroutine subtruct_crs_matvec_33 (NP, N, NPL, NPU,             &
!     &           INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine subtruct_crs_matvec_3x33                             &
!     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,      &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
!
!       subroutine cal_gauss_zeidel_fw_33 (NP, N, NPL, NPU,             &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_gauss_zeidel_bw_33 (NP, N, NPL, NPU,             &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_jacob_fw_33 (NP, N, NPL, NPU,                    &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine cal_jacobi_bw_33 (NP, N, NPL, NPU,                   &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
      module crs_matrix_calcs_33
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
       subroutine cal_crs_matvec_33                                     &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1, W2, W3
!
!
      do j= 1, N
        W1 = D(1,1,j)*X(3*j-2) + D(1,2,j)*X(3*j-1) + D(1,3,j)*X(3*j)
        W2 = D(2,1,j)*X(3*j-2) + D(2,2,j)*X(3*j-1) + D(2,3,j)*X(3*j)
        W3 = D(3,1,j)*X(3*j-2) + D(3,2,j)*X(3*j-1) + D(3,3,j)*X(3*j)
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          W1 = W1 + AU(1,1,i)*X(3*k-2)                                  &
     &            + AU(1,2,i)*X(3*k-1)                                  &
     &            + AU(1,3,i)*X(3*k  )
          W2 = W2 + AU(2,1,i)*X(3*k-2)                                  &
     &            + AU(2,2,i)*X(3*k-1)                                  &
     &            + AU(2,3,i)*X(3*k  )
          W3 = W3 + AU(3,1,i)*X(3*k-2)                                  &
     &            + AU(3,2,i)*X(3*k-1)                                  &
     &            + AU(3,3,i)*X(3*k  )
        end do
!
        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
          W1 = W1 + AL(1,1,i)*X(3*k-2)                                  &
     &            + AL(1,2,i)*X(3*k-1)                                  &
     &            + AL(1,3,i)*X(3*k  )
          W2 = W2 + AL(2,1,i)*X(3*k-2)                                  &
     &            + AL(2,2,i)*X(3*k-1)                                  &
     &            + AL(2,3,i)*X(3*k  )
          W3 = W3 + AL(3,1,i)*X(3*k-2)                                  &
     &            + AL(3,2,i)*X(3*k-1)                                  &
     &            + AL(3,3,i)*X(3*k  )
        end do
        S(3*j-2) = W1
        S(3*j-1) = W2
        S(3*j  ) = W3
      end do
!
      end subroutine cal_crs_matvec_33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_crs_matvec_3x33                                   &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,       &
     &            S1, S2, S3, X1, X2, X3)
!
      integer(kind = kint), intent(in) :: NP, N, NPL, NPU
      integer(kind = kint), intent(in) :: INL(0:NP)
      integer(kind = kint), intent(in) :: INU(0:NP)
      integer(kind = kint), intent(in) :: IAL(NPL)
      integer(kind = kint), intent(in) :: IAU(NPU)
!
      real(kind = kreal), intent(in) :: D(3,3,NP)
      real(kind = kreal), intent(in) :: AL(3,3,NPL)
      real(kind = kreal), intent(in) :: AU(3,3,NPU)
      real(kind = kreal), intent(in) :: X1(3*NP), X2(3*NP), X3(3*NP)
!
      real(kind=kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1, W2, W3
      real(kind = kreal) :: V1, V2, V3
      real(kind = kreal) :: U1, U2, U3
!
!
      do j= 1, N
        W1 = D(1,1,j)*X1(3*j-2) + D(1,2,j)*X1(3*j-1) + D(1,3,j)*X1(3*j)
        W2 = D(2,1,j)*X1(3*j-2) + D(2,2,j)*X1(3*j-1) + D(2,3,j)*X1(3*j)
        W3 = D(3,1,j)*X1(3*j-2) + D(3,2,j)*X1(3*j-1) + D(3,3,j)*X1(3*j)
        V1 = D(1,1,j)*X2(3*j-2) + D(1,2,j)*X2(3*j-1) + D(1,3,j)*X2(3*j)
        V2 = D(2,1,j)*X2(3*j-2) + D(2,2,j)*X2(3*j-1) + D(2,3,j)*X2(3*j)
        V3 = D(3,1,j)*X2(3*j-2) + D(3,2,j)*X2(3*j-1) + D(3,3,j)*X2(3*j)
        U1 = D(1,1,j)*X3(3*j-2) + D(1,2,j)*X3(3*j-1) + D(1,3,j)*X3(3*j)
        U2 = D(2,1,j)*X3(3*j-2) + D(2,2,j)*X3(3*j-1) + D(2,3,j)*X3(3*j)
        U3 = D(3,1,j)*X3(3*j-2) + D(3,2,j)*X3(3*j-1) + D(3,3,j)*X3(3*j)
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
!
          W1 = W1 + AU(1,1,i)*X1(3*k-2)                                 &
     &            + AU(1,2,i)*X1(3*k-1)                                 &
     &            + AU(1,3,i)*X1(3*k  )
          W2 = W2 + AU(2,1,i)*X1(3*k-2)                                 &
     &            + AU(2,2,i)*X1(3*k-1)                                 &
     &            + AU(2,3,i)*X1(3*k  )
          W3 = W3 + AU(3,1,i)*X1(3*k-2)                                 &
     &            + AU(3,2,i)*X1(3*k-1)                                 &
     &            + AU(3,3,i)*X1(3*k  )
!
          V1 = V1 + AU(1,1,i)*X2(3*k-2)                                 &
     &            + AU(1,2,i)*X2(3*k-1)                                 &
     &            + AU(1,3,i)*X2(3*k  )
          V2 = V2 + AU(2,1,i)*X2(3*k-2)                                 &
     &            + AU(2,2,i)*X2(3*k-1)                                 &
     &            + AU(2,3,i)*X2(3*k  )
          V3 = V3 + AU(3,1,i)*X2(3*k-2)                                 &
     &            + AU(3,2,i)*X2(3*k-1)                                 &
     &            + AU(3,3,i)*X2(3*k  )
!
          U1 = U1 + AU(1,1,i)*X3(3*k-2)                                 &
     &            + AU(1,2,i)*X3(3*k-1)                                 &
     &            + AU(1,3,i)*X3(3*k  )
          U2 = U2 + AU(2,1,i)*X3(3*k-2)                                 &
     &            + AU(2,2,i)*X3(3*k-1)                                 &
     &            + AU(2,3,i)*X3(3*k  )
          U3 = U3 + AU(3,1,i)*X3(3*k-2)                                 &
     &            + AU(3,2,i)*X3(3*k-1)                                 &
     &            + AU(3,3,i)*X3(3*k  )
        end do

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
!
          W1 = W1 + AL(1,1,i)*X1(3*k-2)                                 &
     &            + AL(1,2,i)*X1(3*k-1)                                 &
     &            + AL(1,3,i)*X1(3*k  )
          W2 = W2 + AL(2,1,i)*X1(3*k-2)                                 &
     &            + AL(2,2,i)*X1(3*k-1)                                 &
     &            + AL(2,3,i)*X1(3*k  )
          W3 = W3 + AL(3,1,i)*X1(3*k-2)                                 &
     &            + AL(3,2,i)*X1(3*k-1)                                 &
     &            + AL(3,3,i)*X1(3*k  )
!
          V1 = V1 + AL(1,1,i)*X2(3*k-2)                                 &
     &            + AL(1,2,i)*X2(3*k-1)                                 &
     &            + AL(1,3,i)*X2(3*k  )
          V2 = V2 + AL(2,1,i)*X2(3*k-2)                                 &
     &            + AL(2,2,i)*X2(3*k-1)                                 &
     &            + AL(2,3,i)*X2(3*k  )
          V3 = V3 + AL(3,1,i)*X2(3*k-2)                                 &
     &            + AL(3,2,i)*X2(3*k-1)                                 &
     &            + AL(3,3,i)*X2(3*k  )
!
          U1 = U1 + AL(1,1,i)*X3(3*k-2)                                 &
     &            + AL(1,2,i)*X3(3*k-1)                                 &
     &            + AL(1,3,i)*X3(3*k  )
          U2 = U2 + AL(2,1,i)*X3(3*k-2)                                 &
     &            + AL(2,2,i)*X3(3*k-1)                                 &
     &            + AL(2,3,i)*X3(3*k  )
          U3 = U3 + AL(3,1,i)*X3(3*k-2)                                 &
     &            + AL(3,2,i)*X3(3*k-1)                                 &
     &            + AL(3,3,i)*X3(3*k  )
        end do
!
        S1(3*j-2) = W1
        S1(3*j-1) = W2
        S1(3*j  ) = W3
        S2(3*j-2) = V1
        S2(3*j-1) = V2
        S2(3*j  ) = V3
        S3(3*j-2) = U1
        S3(3*j-1) = U2
        S3(3*j  ) = U3
      end do
!
       end subroutine cal_crs_matvec_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_33 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1, W2,  W3
!
!
      do j= 1, N
        W1 = B(3*j-2)                                                   &
     &      - D(1,1,j)*X(3*j-2) - D(1,2,j)*X(3*j-1) - D(1,3,j)*X(3*j)
        W2 = B(3*j-1)                                                   &
     &      - D(2,1,j)*X(3*j-2) - D(2,2,j)*X(3*j-1) - D(2,3,j)*X(3*j)
        W3 = B(3*j  )                                                   &
     &      - D(3,1,j)*X(3*j-2) - D(3,2,j)*X(3*j-1) - D(3,3,j)*X(3*j)
        isU= INU(j-1) + 1
        ieU= INU(j  ) 

        do i= isU, ieU
          k= IAU(i)
          W1 = W1 - AU(1,1,i)*X(3*k-2)                                  &
     &            - AU(1,2,i)*X(3*k-1)                                  &
     &            - AU(1,3,i)*X(3*k  )
          W2 = W2 - AU(2,1,i)*X(3*k-2)                                  &
     &            - AU(2,2,i)*X(3*k-1)                                  &
     &            - AU(2,3,i)*X(3*k  )
          W3 = W3 - AU(3,1,i)*X(3*k-2)                                  &
     &            - AU(3,2,i)*X(3*k-1)                                  &
     &            - AU(3,3,i)*X(3*k  )
        end do

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
          W1 = W1 - AL(1,1,i)*X(3*k-2)                                  &
     &            - AL(1,2,i)*X(3*k-1)                                  &
     &            - AL(1,3,i)*X(3*k  )
          W2 = W2 - AL(2,1,i)*X(3*k-2)                                  &
     &            - AL(2,2,i)*X(3*k-1)                                  &
     &            - AL(2,3,i)*X(3*k  )
          W3 = W3 - AL(3,1,i)*X(3*k-2)                                  &
     &            - AL(3,2,i)*X(3*k-1)                                  &
     &            - AL(3,3,i)*X(3*k  )
        end do
        S(3*j-2) = W1
        S(3*j-1) = W2
        S(3*j  ) = W3
      end do
!
      end subroutine subtruct_crs_matvec_33
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_3x33                              &
     &           (NP, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,       &
     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
      integer(kind = kint), intent(in) :: NP, N, NPL, NPU
      integer(kind = kint), intent(in) :: INL(0:NP)
      integer(kind = kint), intent(in) :: INU(0:NP)
      integer(kind = kint), intent(in) :: IAL(NPL)
      integer(kind = kint), intent(in) :: IAU(NPU)
!
      real(kind = kreal), intent(in) :: D(3,3,NP)
      real(kind = kreal), intent(in) :: AL(3,3,NPL)
      real(kind = kreal), intent(in) :: AU(3,3,NPU)
      real(kind = kreal), intent(in) :: B1(3*NP), B2(3*NP), B3(3*NP)
      real(kind = kreal), intent(in) :: X1(3*NP), X2(3*NP), X3(3*NP)
!
      real(kind=kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: W1, W2, W3
      real(kind = kreal) :: V1, V2, V3
      real(kind = kreal) :: U1, U2, U3
!
!
      do j= 1, N
        W1 = B1(3*j-2)                                                  &
     &     - D(1,1,j)*X1(3*j-2) - D(1,2,j)*X1(3*j-1) - D(1,3,j)*X1(3*j)
        W2 = B1(3*j-1)                                                  &
     &     - D(2,1,j)*X1(3*j-2) - D(2,2,j)*X1(3*j-1) - D(2,3,j)*X1(3*j)
        W3 = B1(3*j  )                                                  &
     &     - D(3,1,j)*X1(3*j-2) - D(3,2,j)*X1(3*j-1) - D(3,3,j)*X1(3*j)
        V1 = B2(3*j-2)                                                  &
     &     - D(1,1,j)*X2(3*j-2) - D(1,2,j)*X2(3*j-1) - D(1,3,j)*X2(3*j)
        V2 = B2(3*j-1)                                                  &
     &     - D(2,1,j)*X2(3*j-2) - D(2,2,j)*X2(3*j-1) - D(2,3,j)*X2(3*j)
        V3 = B2(3*j  )                                                  &
     &     - D(3,1,j)*X2(3*j-2) - D(3,2,j)*X2(3*j-1) - D(3,3,j)*X2(3*j)
        U1 = B3(3*j-2)                                                  &
     &     - D(1,1,j)*X3(3*j-2) - D(1,2,j)*X3(3*j-1) - D(1,3,j)*X3(3*j)
        U2 = B3(3*j-1)                                                  &
     &     - D(2,1,j)*X3(3*j-2) - D(2,2,j)*X3(3*j-1) - D(2,3,j)*X3(3*j)
        U3 = B3(3*j  )                                                  &
     &     - D(3,1,j)*X3(3*j-2) - D(3,2,j)*X3(3*j-1) - D(3,3,j)*X3(3*j)
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          W1 = W1 - AU(1,1,i)*X1(3*k-2)                                 &
     &            - AU(1,2,i)*X1(3*k-1)                                 &
     &            - AU(1,3,i)*X1(3*k  )
          W2 = W2 - AU(2,1,i)*X1(3*k-2)                                 &
     &            - AU(2,2,i)*X1(3*k-1)                                 &
     &            - AU(2,3,i)*X1(3*k  )
          W3 = W3 - AU(3,1,i)*X1(3*k-2)                                 &
     &            - AU(3,2,i)*X1(3*k-1)                                 &
     &            - AU(3,3,i)*X1(3*k  )
!
          V1 = V1 - AU(1,1,i)*X2(3*k-2)                                 &
     &            - AU(1,2,i)*X2(3*k-1)                                 &
     &            - AU(1,3,i)*X2(3*k  )
          V2 = V2 - AU(2,1,i)*X2(3*k-2)                                 &
     &            - AU(2,2,i)*X2(3*k-1)                                 &
     &            - AU(2,3,i)*X2(3*k  )
          V3 = V3 - AU(3,1,i)*X2(3*k-2)                                 &
     &            - AU(3,2,i)*X2(3*k-1)                                 &
     &            - AU(3,3,i)*X2(3*k  )
!
          U1 = U1 - AU(1,1,i)*X3(3*k-2)                                 &
     &            - AU(1,2,i)*X3(3*k-1)                                 &
     &            - AU(1,3,i)*X3(3*k  )
          U2 = U2 - AU(2,1,i)*X3(3*k-2)                                 &
     &            - AU(2,2,i)*X3(3*k-1)                                 &
     &            - AU(2,3,i)*X3(3*k  )
          U3 = U3 - AU(3,1,i)*X3(3*k-2)                                 &
     &            - AU(3,2,i)*X3(3*k-1)                                 &
     &            - AU(3,3,i)*X3(3*k  )
        end do

        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k= IAL(i)
          W1 = W1 - AL(1,1,i)*X1(3*k-2)                                 &
     &            - AL(1,2,i)*X1(3*k-1)                                 &
     &            - AL(1,3,i)*X1(3*k  )
          W2 = W2 - AL(2,1,i)*X1(3*k-2)                                 &
     &            - AL(2,2,i)*X1(3*k-1)                                 &
     &            - AL(2,3,i)*X1(3*k  )
          W3 = W3 - AL(3,1,i)*X1(3*k-2)                                 &
     &            - AL(3,2,i)*X1(3*k-1)                                 &
     &            - AL(3,3,i)*X1(3*k  )
!
          V1 = V1 - AL(1,1,i)*X2(3*k-2)                                 &
     &            - AL(1,2,i)*X2(3*k-1)                                 &
     &            - AL(1,3,i)*X2(3*k  )
          V2 = V2 - AL(2,1,i)*X2(3*k-2)                                 &
     &            - AL(2,2,i)*X2(3*k-1)                                 &
     &            - AL(2,3,i)*X2(3*k  )
          V3 = V3 - AL(3,1,i)*X2(3*k-2)                                 &
     &            - AL(3,2,i)*X2(3*k-1)                                 &
     &            - AL(3,3,i)*X2(3*k  )
!
          U1 = U1 - AL(1,1,i)*X3(3*k-2)                                 &
     &            - AL(1,2,i)*X3(3*k-1)                                 &
     &            - AL(1,3,i)*X3(3*k  )
          U2 = U2 - AL(2,1,i)*X3(3*k-2)                                 &
     &            - AL(2,2,i)*X3(3*k-1)                                 &
     &            - AL(2,3,i)*X3(3*k  )
          U3 = U3 - AL(3,1,i)*X3(3*k-2)                                 &
     &            - AL(3,2,i)*X3(3*k-1)                                 &
     &            - AL(3,3,i)*X3(3*k  )
        end do
!
        S1(3*j-2) = W1
        S1(3*j-1) = W2
        S1(3*j  ) = W3
        S2(3*j-2) = V1
        S2(3*j-1) = V2
        S2(3*j  ) = V3
        S3(3*j-2) = U1
        S3(3*j-1) = U2
        S3(3*j  ) = U3
      end do

       end subroutine subtruct_crs_matvec_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_fw_33 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v1, v2, v3
!
!
       DO i = 1, N
          v1 = B(3*i-2)
          v2 = B(3*i-1)
          v3 = B(3*i  )
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v1 = v1 - AL(1,1,j) * S(3*k-2)                             &
     &               - AL(1,2,j) * S(3*k-1)                             &
     &               - AL(1,3,j) * S(3*k  )
             v2 = v2 - AL(2,1,j) * S(3*k-2)                             &
     &               - AL(2,2,j) * S(3*k-1)                             &
     &               - AL(2,3,j) * S(3*k  )
             v3 = v3 - AL(2,1,j) * S(3*k-2)                             &
     &               - AL(2,2,j) * S(3*k-1)                             &
     &               - AL(2,3,j) * S(3*k  )
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v1 = v1 - AU(1,1,j) * S(3*k-2)                             &
     &               - AU(1,2,j) * S(3*k-1)                             &
     &               - AU(1,3,j) * S(3*k  )
             v2 = v2 - AU(2,1,j) * S(3*k-2)                             &
     &               - AU(2,2,j) * S(3*k-1)                             &
     &               - AU(2,3,j) * S(3*k  )
             v3 = v3 - AU(3,1,j) * S(3*k-2)                             &
     &               - AU(3,2,j) * S(3*k-1)                             &
     &               - AU(3,3,j) * S(3*k  )
          END DO

          v1 = v1 - D(1,2,i) * S(3*i-1) - D(1,3,i) * S(3*i  )
          S(3*i-2) = v1 / D(1,1,i)
          v2 = v2 - D(2,1,i) * S(3*i-2) - D(2,3,i) * S(3*i  )
          S(3*i-1) = v2 / D(2,2,i)
          v3 = v3 - D(2,1,i) * S(3*i-2) - D(2,2,i) * S(3*i-1)
          S(3*i  ) = v3 / D(3,3,i)
       END DO
!
      end subroutine cal_gauss_zeidel_fw_33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_bw_33 (NP, N, NPL, NPU,              &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v1, v2, v3
!
!
       DO i = N, 1, -1
          v1 = B(3*i-2)
          v2 = B(3*i-1)
          v3 = B(3*i  )
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v1 = v1 - AL(1,1,j) * S(3*k-2)                             &
     &               - AL(1,2,j) * S(3*k-1)                             &
     &               - AL(1,3,j) * S(3*k  )
             v2 = v2 - AL(2,1,j) * S(3*k-2)                             &
     &               - AL(2,2,j) * S(3*k-1)                             &
     &               - AL(2,3,j) * S(3*k  )
             v3 = v3 - AL(2,1,j) * S(3*k-2)                             &
     &               - AL(2,2,j) * S(3*k-1)                             &
     &               - AL(2,3,j) * S(3*k  )
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v1 = v1 - AU(1,1,j) * S(3*k-2)                             &
     &               - AU(1,2,j) * S(3*k-1)                             &
     &               - AU(1,3,j) * S(3*k  )
             v2 = v2 - AU(2,1,j) * S(3*k-2)                             &
     &               - AU(2,2,j) * S(3*k-1)                             &
     &               - AU(2,3,j) * S(3*k  )
             v3 = v3 - AU(3,1,j) * S(3*k-2)                             &
     &               - AU(3,2,j) * S(3*k-1)                             &
     &               - AU(3,3,j) * S(3*k  )
          END DO

          v1 = v1 - D(1,2,i) * S(3*i-1) - D(1,3,i) * S(3*i  )
          S(3*i-2) = v1 / D(1,1,i)
          v2 = v2 - D(2,1,i) * S(3*i-2) - D(2,3,i) * S(3*i  )
          S(3*i-1) = v2 / D(2,2,i)
          v3 = v3 - D(2,1,i) * S(3*i-2) - D(2,2,i) * S(3*i-1)
          S(3*i  ) = v3 / D(3,3,i)
       END DO
!
      end subroutine cal_gauss_zeidel_bw_33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacob_fw_33 (NP, N, NPL, NPU,                     &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v1, v2, v3
!
!
       DO i = 1, N
          v1 = B(3*i-2)
          v2 = B(3*i-1)
          v3 = B(3*i  )
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v1 = v1 - AL(1,1,j) * X(3*k-2)                             &
     &               - AL(1,2,j) * X(3*k-1)                             &
     &               - AL(1,3,j) * X(3*k  )
             v2 = v2 - AL(2,1,j) * X(3*k-2)                             &
     &               - AL(2,2,j) * X(3*k-1)                             &
     &               - AL(2,3,j) * X(3*k  )
             v3 = v3 - AL(2,1,j) * X(3*k-2)                             &
     &               - AL(2,2,j) * X(3*k-1)                             &
     &               - AL(2,3,j) * X(3*k  )
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v1 = v1 - AU(1,1,j) * X(3*k-2)                             &
     &               - AU(1,2,j) * X(3*k-1)                             &
     &               - AU(1,3,j) * X(3*k  )
             v2 = v2 - AU(2,1,j) * X(3*k-2)                             &
     &               - AU(2,2,j) * X(3*k-1)                             &
     &               - AU(2,3,j) * X(3*k  )
             v3 = v3 - AU(3,1,j) * X(3*k-2)                             &
     &               - AU(3,2,j) * X(3*k-1)                             &
     &               - AU(3,3,j) * X(3*k  )
          END DO

          v1 = v1 - D(1,2,i) * X(3*i-1) - D(1,3,i) * X(3*i  )
          v2 = v2 - D(2,1,i) * X(3*i-2) - D(2,3,i) * X(3*i  )
          v3 = v3 - D(2,1,i) * X(3*i-2) - D(2,2,i) * X(3*i-1)
          S(3*i-2) = v1 / D(1,1,i)
          S(3*i-1) = v2 / D(2,2,i)
          S(3*i  ) = v3 / D(3,3,i)
       END DO
!
      end subroutine cal_jacob_fw_33
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacobi_bw_33 (NP, N, NPL, NPU,                    &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(3*NP)
       real(kind = kreal), intent(in) :: X(3*NP)
       real(kind = kreal), intent(in) :: D(3,3,NP)
       real(kind = kreal), intent(in) :: AL(3,3,NPL)
       real(kind = kreal), intent(in) :: AU(3,3,NPU)
!
       real(kind = kreal), intent(inout) :: S(3*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL
      real(kind = kreal) :: v1, v2, v3
!
!
       DO i = N, 1, -1
          v1 = B(3*i-2)
          v2 = B(3*i-1)
          v3 = B(3*i  )
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
             k = IAL(j)
             v1 = v1 - AL(1,1,j) * X(3*k-2)                             &
     &               - AL(1,2,j) * X(3*k-1)                             &
     &               - AL(1,3,j) * X(3*k  )
             v2 = v2 - AL(2,1,j) * X(3*k-2)                             &
     &               - AL(2,2,j) * X(3*k-1)                             &
     &               - AL(2,3,j) * X(3*k  )
             v3 = v3 - AL(2,1,j) * X(3*k-2)                             &
     &               - AL(2,2,j) * X(3*k-1)                             &
     &               - AL(2,3,j) * X(3*k  )
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
             k = IAU(j)
             v1 = v1 - AU(1,1,j) * X(3*k-2)                             &
     &               - AU(1,2,j) * X(3*k-1)                             &
     &               - AU(1,3,j) * X(3*k  )
             v2 = v2 - AU(2,1,j) * X(3*k-2)                             &
     &               - AU(2,2,j) * X(3*k-1)                             &
     &               - AU(2,3,j) * X(3*k  )
             v3 = v3 - AU(3,1,j) * X(3*k-2)                             &
     &               - AU(3,2,j) * X(3*k-1)                             &
     &               - AU(3,3,j) * X(3*k  )
          END DO

          v1 = v1 - D(1,2,i) * X(3*i-1) - D(1,3,i) * X(3*i  )
          v2 = v2 - D(2,1,i) * X(3*i-2) - D(2,3,i) * X(3*i  )
          v3 = v3 - D(2,1,i) * X(3*i-2) - D(2,2,i) * X(3*i-1)
          S(3*i-2) = v1 / D(1,1,i)
          S(3*i-1) = v2 / D(2,2,i)
          S(3*i  ) = v3 / D(3,3,i)
       END DO
!
      end subroutine cal_jacobi_bw_33
!
!  ---------------------------------------------------------------------
!
      end module crs_matrix_calcs_33
