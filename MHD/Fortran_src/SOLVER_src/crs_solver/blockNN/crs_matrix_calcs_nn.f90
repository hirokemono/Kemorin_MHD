!
!      module crs_matrix_calcs_nn
!
!     Written by Kemorin
!
!
!C +-------------+
!C | {S}= [A]{X} |
!C +-------------+
!       subroutine cal_crs_matvec_nn                                    &
!     &      (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!       subroutine cal_crs_matvec_3xnn                                  &
!     &           (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,  &
!     &            S1, S2, S3, X1, X2, X3)
!
!C +-------------- ----+
!C | {S}= {B} - [A]{X} |
!C +-------------------+
!       subroutine subtruct_crs_matvec_nn (NP, NB, N, NPL, NPU,         &
!     &           INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine subtruct_crs_matvec_3xnn                             &
!     &           (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,  &
!     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
!
!       subroutine cal_gauss_zeidel_fw_nn (NP, NB, N, NPL, NPU,         &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_gauss_zeidel_bw_nn (NP, NB, N, NPL, NPU,         &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!       subroutine cal_jacob_fw_nn (NP, NB, N, NPL, NPU,                &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!       subroutine cal_jacobi_bw_nn (NP, NB, N, NPL, NPU,               &
!     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
      module crs_matrix_calcs_nn
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
       subroutine cal_crs_matvec_nn                                     &
     &      (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU, S, X)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: X(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
!
!
      do j= 1, N
        do n1 = 1, NB
          S(NB*(j-1)+n1) = 0.0d0
        end do
!
        do n1 = 1, NB
          do n2 = 1, NB
            S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                             &
     &                       + D(n1,n2,j)*X( NB*(j-1)+n2 )
          end do
        end do
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                           &
     &                        + AU(n1,n2,i)*X( NB*(k-1)+n2 )
            end do
          end do
        end do
!
        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k = IAL(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                           &
     &                        + AL(n1,n2,i)*X( NB*(k-1)+n2 )
            end do
          end do
        end do
!
      end do
!
      end subroutine cal_crs_matvec_nn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_crs_matvec_3xnn                                   &
     &       (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,       &
     &        S1, S2, S3, X1, X2, X3)
!
      integer(kind = kint), intent(in) :: NP, NB, N, NPL, NPU
      integer(kind = kint), intent(in) :: INL(0:NP)
      integer(kind = kint), intent(in) :: INU(0:NP)
      integer(kind = kint), intent(in) :: IAL(NPL)
      integer(kind = kint), intent(in) :: IAU(NPU)
!
      real(kind = kreal), intent(in) :: D(NB,NB,NP)
      real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
      real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
      real(kind = kreal), intent(in) :: X1(NB*NP), X2(NB*NP), X3(NB*NP)
!
      real(kind=kreal), intent(inout) :: S1(NB*NP), S2(NB*NP), S3(NB*NP)
!
      integer(kind = kint) :: k, i, j, ist, ied, n1, n2
!
!
      do j= 1, N
        do n1 = 1, NB
          S1(NB*(j-1)+n1) = 0.0d0
          S2(NB*(j-1)+n1) = 0.0d0
          S3(NB*(j-1)+n1) = 0.0d0
        end do
!
        do n1 = 1, NB
          do n2 = 1, NB
            S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                           &
     &                       + D(n1,n2,j)*X1( NB*(j-1)+n2 )
            S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                           &
     &                       + D(n1,n2,j)*X2( NB*(j-1)+n2 )
            S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                           &
     &                       + D(n1,n2,j)*X3( NB*(j-1)+n2 )
          end do
        end do
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                         &
     &                        + AU(n1,n2,i)*X1( NB*(k-1)+n2 )
              S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                         &
     &                        + AU(n1,n2,i)*X2( NB*(k-1)+n2 )
              S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                         &
     &                        + AU(n1,n2,i)*X3( NB*(k-1)+n2 )
            end do
          end do
        end do
!
        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k = IAL(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                         &
     &                        + AL(n1,n2,i)*X1( NB*(k-1)+n2 )
              S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                         &
     &                        + AL(n1,n2,i)*X2( NB*(k-1)+n2 )
              S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                         &
     &                        + AL(n1,n2,i)*X3( NB*(k-1)+n2 )
            end do
          end do
        end do
!
      end do
!
       end subroutine cal_crs_matvec_3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_nn (NP, NB, N, NPL, NPU,          &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(in) :: X(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
      real(kind = kreal) :: W1, W2,  W3
!
!
      do j= 1, N
        do n1 = 1, NB
          S(NB*(j-1)+n1) = B(NB*(j-1)+n1)
        end do
!
        do n1 = 1, NB
          do n2 = 1, NB
            S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                             &
     &                       - D(n1,n2,j)*X( NB*(j-1)+n2 )
          end do
        end do
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                           &
     &                        - AU(n1,n2,i)*X( NB*(k-1)+n2 )
            end do
          end do
        end do
!
        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k = IAL(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S(NB*(j-1)+n1) = S(NB*(j-1)+n1)                           &
     &                        - AL(n1,n2,i)*X( NB*(k-1)+n2 )
            end do
          end do
        end do
!
      end do
!
      end subroutine subtruct_crs_matvec_nn
!
!  ---------------------------------------------------------------------
!
       subroutine subtruct_crs_matvec_3xnn                              &
     &           (NP, NB, N, NPL, NPU, INL, INU, IAL, IAU, D, AL, AU,   &
     &            S1, S2, S3, B1, B2, B3, X1, X2, X3)
!
      integer(kind = kint), intent(in) :: NP, NB, N, NPL, NPU
      integer(kind = kint), intent(in) :: INL(0:NP)
      integer(kind = kint), intent(in) :: INU(0:NP)
      integer(kind = kint), intent(in) :: IAL(NPL)
      integer(kind = kint), intent(in) :: IAU(NPU)
!
      real(kind = kreal), intent(in) :: D(NB,NB,NP)
      real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
      real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
      real(kind = kreal), intent(in) :: B1(NB*NP), B2(NB*NP), B3(NB*NP)
      real(kind = kreal), intent(in) :: X1(NB*NP), X2(NB*NP), X3(NB*NP)
!
      real(kind=kreal), intent(inout) :: S1(NB*NP), S2(NB*NP), S3(NB*NP)
!
      integer(kind = kint) :: k, i, j, ist, ied, n1, n2
      real(kind = kreal) :: W1, W2, W3
      real(kind = kreal) :: V1, V2, V3
      real(kind = kreal) :: U1, U2, U3
!
!
      do j= 1, N
        do n1 = 1, NB
          S1(NB*(j-1)+n1) = B1(NB*(j-1)+n1)
          S2(NB*(j-1)+n1) = B2(NB*(j-1)+n1)
          S3(NB*(j-1)+n1) = B3(NB*(j-1)+n1)
        end do
!
        do n1 = 1, NB
          do n2 = 1, NB
            S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                           &
     &                       - D(n1,n2,j)*X1( NB*(j-1)+n2 )
            S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                           &
     &                       - D(n1,n2,j)*X2( NB*(j-1)+n2 )
            S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                           &
     &                       - D(n1,n2,j)*X3( NB*(j-1)+n2 )
          end do
        end do
!
        isU= INU(j-1) + 1
        ieU= INU(j  ) 
        do i= isU, ieU
          k= IAU(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                         &
     &                        - AU(n1,n2,i)*X1( NB*(k-1)+n2 )
              S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                         &
     &                        - AU(n1,n2,i)*X2( NB*(k-1)+n2 )
              S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                         &
     &                        - AU(n1,n2,i)*X3( NB*(k-1)+n2 )
            end do
          end do
        end do
!
        isL= INL(j-1) + 1
        ieL= INL(j  ) 
        do i= isL, ieL
          k = IAL(i)
          do n1 = 1, NB
            do n2 = 1, NB
              S1(NB*(j-1)+n1) = S1(NB*(j-1)+n1)                         &
     &                        - AL(n1,n2,i)*X1( NB*(k-1)+n2 )
              S2(NB*(j-1)+n1) = S2(NB*(j-1)+n1)                         &
     &                        - AL(n1,n2,i)*X2( NB*(k-1)+n2 )
              S3(NB*(j-1)+n1) = S3(NB*(j-1)+n1)                         &
     &                        - AL(n1,n2,i)*X3( NB*(k-1)+n2 )
            end do
          end do
        end do
!
      end do

       end subroutine subtruct_crs_matvec_3xnn
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_fw_nn (NP, NB, N, NPL, NPU,          &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
!
!
       DO i = 1, N
         do n1 = 1, NB
           S( NB*(i-1)+n1 ) = B( NB*(i-1)+n1 )
         end do
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
            k = IAL(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AL(n1,n2,i) * S(NB*(k-1)+n2)
              end do
            end do
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
            k = IAU(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AU(n1,n2,i) * S(NB*(k-1)+n2)
              end do
            end do
          END DO
!
          do n1 = 1, NB
            do n2 = 1, n1-1
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * S(NB*(i-1)+n2)
            end do
            do n2 = n1+1, NB
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * S(NB*(i-1)+n2)
            end do
            S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 ) / D(n1,n1,i)
          end do
       END DO
!
      end subroutine cal_gauss_zeidel_fw_nn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_gauss_zeidel_bw_nn (NP, NB, N, NPL, NPU,          &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
!
!
       DO i = N, 1, -1
         do n1 = 1, NB
           S( NB*(i-1)+n1 ) = B( NB*(i-1)+n1 )
         end do
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
            k = IAL(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AL(n1,n2,i) * S(NB*(k-1)+n2)
              end do
            end do
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
            k = IAU(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AU(n1,n2,i) * S(NB*(k-1)+n2)
              end do
            end do
          END DO
!
          do n1 = 1, NB
            do n2 = 1, n1-1
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * S(NB*(i-1)+n2)
            end do
            do n2 = n1+1, NB
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * S(NB*(i-1)+n2)
            end do
            S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 ) / D(n1,n1,i)
          end do
       END DO
!
      end subroutine cal_gauss_zeidel_bw_nn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacob_fw_nn (NP, NB, N, NPL, NPU,                 &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(in) :: X(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
!
!
       DO i = 1, N
         do n1 = 1, NB
           S( NB*(i-1)+n1 ) = B( NB*(i-1)+n1 )
         end do
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
            k = IAL(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AL(n1,n2,i) * X(NB*(k-1)+n2)
              end do
            end do
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
            k = IAU(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AU(n1,n2,i) * X(NB*(k-1)+n2)
              end do
            end do
          END DO
!
          do n1 = 1, NB
            do n2 = 1, n1-1
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * X(NB*(i-1)+n2)
            end do
            do n2 = n1+1, NB
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * X(NB*(i-1)+n2)
            end do
            S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 ) / D(n1,n1,i)
          end do
       END DO
!
      end subroutine cal_jacob_fw_nn
!
!  ---------------------------------------------------------------------
!
       subroutine cal_jacobi_bw_nn (NP, NB, N, NPL, NPU,                &
     &     INL, INU, IAL, IAU, D, AL, AU, S, B, X)
!
       integer(kind = kint), intent(in) :: NP, NB, N
       integer(kind = kint), intent(in) :: NPL, NPU
       integer(kind = kint), intent(in) :: INL(0:NP)
       integer(kind = kint), intent(in) :: INU(0:NP)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
!
       real(kind = kreal), intent(in) :: B(NB*NP)
       real(kind = kreal), intent(in) :: X(NB*NP)
       real(kind = kreal), intent(in) :: D(NB,NB,NP)
       real(kind = kreal), intent(in) :: AL(NB,NB,NPL)
       real(kind = kreal), intent(in) :: AU(NB,NB,NPU)
!
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
      integer(kind = kint) :: i, j, k, isU, ieU, isL, ieL, n1, n2
      real(kind = kreal) :: v
!
!
       DO i = N, 1, -1
         do n1 = 1, NB
           S( NB*(i-1)+n1 ) = B( NB*(i-1)+n1 )
         end do
!
          isL = INL(i-1) + 1
          ieL = INL(i)
          DO j = isL, ieL
            k = IAL(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AL(n1,n2,i) * X(NB*(k-1)+n2)
              end do
            end do
          END DO

          isU = INU(i - 1) + 1
          ieU = INU(i)
          DO j = isU, ieU
            k = IAU(j)
            do n1 = 1, NB
              do n2 = 1, n1-1
                S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                     &
     &                            - AU(n1,n2,i) * X(NB*(k-1)+n2)
              end do
            end do
          END DO
!
          do n1 = 1, NB
            do n2 = 1, n1-1
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * X(NB*(i-1)+n2)
            end do
            do n2 = n1+1, NB
              S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 )                       &
     &                          - D(n1,n2,i) * X(NB*(i-1)+n2)
            end do
            S( NB*(i-1)+n1 ) = S( NB*(i-1)+n1 ) / D(n1,n1,i)
          end do
       END DO
!
      end subroutine cal_jacobi_bw_nn
!
!  ---------------------------------------------------------------------
!
      end module crs_matrix_calcs_nn
