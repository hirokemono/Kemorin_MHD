!
!      module cal_4_diagonal_33
!
!     Written by Kemorin
!
!       subroutine set_by_diagonal_33(NP, PEsmpTOT, STACKmcG, S, V, D)
!       subroutine set_by_diagonal_3x33(NP, PEsmpTOT, STACKmcG,         &
!     &           S1, S2, S3, V1, V2, V3, D)
!
!       subroutine subtract_diagonal_33(NP, PEsmpTOT, STACKmcG,         &
!     &           S, B, V, D)
!       subroutine subtract_diagonal_3x33(NP, PEsmpTOT, STACKmcG,       &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
!       subroutine add_diagonal_33(NP, PEsmpTOT, STACKmcG,              &
!     &           S, B, V, D)
!       subroutine add_diagonal_3x33(NP, PEsmpTOT, STACKmcG,            &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
      module cal_4_diagonal_33
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
       subroutine set_by_diagonal_33(NP, PEsmpTOT, STACKmcG, S, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: Xm0,Xm1,Xm2
!
!
!$omp parallel do private(iS,iE,i,Xm0,Xm1,Xm2) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
!voption indep (S,V,D)
        do i= iS, iE
          Xm2= V(3*i-2)
          Xm1= V(3*i-1)
          Xm0= V(3*i  )
          S(3*i-2)= D(9*i-8)*Xm2 + D(9*i-7)*Xm1 + D(9*i-6)*Xm0
          S(3*i-1)= D(9*i-5)*Xm2 + D(9*i-4)*Xm1 + D(9*i-3)*Xm0
          S(3*i  )= D(9*i-2)*Xm2 + D(9*i-1)*Xm1 + D(9*i  )*Xm0
        enddo
      enddo
!$omp end parallel do

!
       end subroutine set_by_diagonal_33
!
!  ---------------------------------------------------------------------
!
       subroutine set_by_diagonal_3x33(NP, PEsmpTOT, STACKmcG,          &
     &           S1, S2, S3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP)
       real(kind = kreal), intent(inout) :: S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
        do i= iS, iE
          S1(3*i-2) =  D(9*i-8)*V1(3*i-2)                               &
     &               + D(9*i-7)*V1(3*i-1)                               &
     &               + D(9*i-6)*V1(3*i  )
          S2(3*i-2) =  D(9*i-8)*V2(3*i-2)                               &
     &               + D(9*i-7)*V2(3*i-1)                               &
     &               + D(9*i-6)*V2(3*i  )
          S3(3*i-2) =  D(9*i-8)*V3(3*i-2)                               &
     &               + D(9*i-7)*V3(3*i-1)                               &
     &               + D(9*i-6)*V3(3*i  )
          S1(3*i-1) =  D(9*i-5)*V1(3*i-2)                               &
     &               + D(9*i-4)*V1(3*i-1)                               &
     &               + D(9*i-3)*V1(3*i  )
          S2(3*i-1) =  D(9*i-5)*V2(3*i-2)                               &
     &               + D(9*i-4)*V2(3*i-1)                               &
     &               + D(9*i-3)*V2(3*i  )
          S3(3*i-1) =  D(9*i-5)*V3(3*i-2)                               &
     &               + D(9*i-4)*V3(3*i-1)                               &
     &               + D(9*i-3)*V3(3*i  )
          S1(3*i  ) =  D(9*i-2)*V1(3*i-2)                               &
     &               + D(9*i-1)*V1(3*i-1)                               &
     &               + D(9*i  )*V1(3*i  )
          S2(3*i  ) =  D(9*i-2)*V2(3*i-2)                               &
     &               + D(9*i-1)*V2(3*i-1)                               &
     &               + D(9*i  )*V2(3*i  )
          S3(3*i  ) =  D(9*i-2)*V3(3*i-2)                               &
     &               + D(9*i-1)*V3(3*i-1)                               &
     &               + D(9*i  )*V3(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine set_by_diagonal_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_33(NP, PEsmpTOT, STACKmcG,          &
     &           S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: B(3*NP), V(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: Xm0,Xm1,Xm2
!
!
!$omp parallel do private(iS,iE,i,Xm0,Xm1,Xm2) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
        do i= iS, iE
          Xm2= V(3*i-2)
          Xm1= V(3*i-1)
          Xm0= V(3*i  )
          S(3*i-2) = B(3*i-2)                                           &
     &              - D(9*i-8)*Xm2 - D(9*i-7)*Xm1 - D(9*i-6)*Xm0
          S(3*i-1) = B(3*i-1)                                           &
     &              - D(9*i-5)*Xm2 - D(9*i-4)*Xm1 - D(9*i-3)*Xm0
          S(3*i  ) = B(3*i  )                                           &
     &              - D(9*i-2)*Xm2 - D(9*i-1)*Xm1 - D(9*i  )*Xm0
        enddo
      enddo
!$omp end parallel do
!
       end subroutine subtract_diagonal_33
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_3x33(NP, PEsmpTOT, STACKmcG,        &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: B1(3*NP), V1(3*NP)
       real(kind = kreal), intent(in) :: B2(3*NP), V2(3*NP)
       real(kind = kreal), intent(in) :: B3(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP)
       real(kind = kreal), intent(inout) :: S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
        do i= iS, iE
          S1(3*i-2) = B1(3*i-2) - D(9*i-8)*V1(3*i-2)                    &
     &                          - D(9*i-7)*V1(3*i-1)                    &
     &                          - D(9*i-6)*V1(3*i  )
          S2(3*i-2) = B2(3*i-2) - D(9*i-8)*V2(3*i-2)                    &
     &                          - D(9*i-7)*V2(3*i-1)                    &
     &                          - D(9*i-6)*V2(3*i  )
          S3(3*i-2) = B3(3*i-2) - D(9*i-8)*V3(3*i-2)                    &
     &                          - D(9*i-7)*V3(3*i-1)                    &
     &                          - D(9*i-6)*V3(3*i  )
          S1(3*i-1) = B1(3*i-1) - D(9*i-5)*V1(3*i-2)                    &
     &                          - D(9*i-4)*V1(3*i-1)                    &
     &                          - D(9*i-3)*V1(3*i  )
          S2(3*i-1) = B2(3*i-1) - D(9*i-5)*V2(3*i-2)                    &
     &                          - D(9*i-4)*V2(3*i-1)                    &
     &                          - D(9*i-3)*V2(3*i  )
          S3(3*i-1) = B3(3*i-1) - D(9*i-5)*V3(3*i-2)                    &
     &                          - D(9*i-4)*V3(3*i-1)                    &
     &                          - D(9*i-3)*V3(3*i  )
          S1(3*i  ) = B1(3*i  ) - D(9*i-2)*V1(3*i-2)                    &
     &                          - D(9*i-1)*V1(3*i-1)                    &
     &                          - D(9*i  )*V1(3*i  )
          S2(3*i  ) = B2(3*i  ) - D(9*i-2)*V2(3*i-2)                    &
     &                          - D(9*i-1)*V2(3*i-1)                    &
     &                          - D(9*i  )*V2(3*i  )
          S3(3*i  ) = B3(3*i  ) - D(9*i-2)*V3(3*i-2)                    &
     &                          - D(9*i-1)*V3(3*i-1)                    &
     &                          - D(9*i  )*V3(3*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine subtract_diagonal_3x33
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_33(NP, PEsmpTOT, STACKmcG, S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: B(3*NP), V(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
       real(kind = kreal) :: Xm0,Xm1,Xm2
!
!
!$omp parallel do private(iS,iE,i,Xm0,Xm1,Xm2) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V)
!CDIR ON_ADB(S)
!cdir nodep
        do i= iS, iE
          Xm2= V(3*i-2)
          Xm1= V(3*i-1)
          Xm0= V(3*i  )
          S(3*i-2) = B(3*i-2)                                           &
     &              + D(9*i-8)*Xm2 + D(9*i-7)*Xm1 + D(9*i-6)*Xm0
          S(3*i-1) = B(3*i-1)                                           &
     &              + D(9*i-5)*Xm2 + D(9*i-4)*Xm1 + D(9*i-3)*Xm0
          S(3*i  ) = B(3*i  )                                           &
     &              + D(9*i-2)*Xm2 + D(9*i-1)*Xm1 + D(9*i  )*Xm0
        enddo
      enddo
!$omp end parallel do

!
       end subroutine add_diagonal_33
!
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_3x33(NP, PEsmpTOT, STACKmcG,             &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(in) :: B1(3*NP), V1(3*NP)
       real(kind = kreal), intent(in) :: B2(3*NP), V2(3*NP)
       real(kind = kreal), intent(in) :: B3(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP)
       real(kind = kreal), intent(inout) :: S2(3*NP)
       real(kind = kreal), intent(inout) :: S3(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!$omp parallel do private(iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!CDIR ON_ADB(V1)
!CDIR ON_ADB(V2)
!CDIR ON_ADB(V3)
!CDIR ON_ADB(S1)
!CDIR ON_ADB(S2)
!CDIR ON_ADB(S3)
!cdir nodep
        do i= iS, iE
          S1(3*i-2) = B1(3*i-2) + D(9*i-8)*V1(3*i-2)                    &
     &                          + D(9*i-7)*V1(3*i-1)                    &
     &                          + D(9*i-6)*V1(3*i  )
          S2(3*i-2) = B2(3*i-2) + D(9*i-8)*V2(3*i-2)                    &
     &                          + D(9*i-7)*V2(3*i-1)                    &
     &                          + D(9*i-6)*V2(3*i  )
          S3(3*i-2) = B3(3*i-2) + D(9*i-8)*V3(3*i-2)                    &
     &                          + D(9*i-7)*V3(3*i-1)                    &
     &                          + D(9*i-6)*V3(3*i  )
          S1(3*i-1) = B1(3*i-1) + D(9*i-5)*V1(3*i-2)                    &
     &                          + D(9*i-4)*V1(3*i-1)                    &
     &                          + D(9*i-3)*V1(3*i  )
          S2(3*i-1) = B2(3*i-1) + D(9*i-5)*V2(3*i-2)                    &
     &                          + D(9*i-4)*V2(3*i-1)                    &
     &                          + D(9*i-3)*V2(3*i  )
          S3(3*i-1) = B3(3*i-1) + D(9*i-5)*V3(3*i-2)                    &
     &                          + D(9*i-4)*V3(3*i-1)                    &
     &                          + D(9*i-3)*V3(3*i  )
          S1(3*i  ) = B1(3*i  ) + D(9*i-2)*V1(3*i-2)                    &
     &                          + D(9*i-1)*V1(3*i-1)                    &
     &                          + D(9*i  )*V1(3*i  )
          S2(3*i  ) = B2(3*i  ) + D(9*i-2)*V2(3*i-2)                    &
     &                          + D(9*i-1)*V2(3*i-1)                    &
     &                          + D(9*i  )*V2(3*i  )
          S3(3*i  ) = B3(3*i  ) + D(9*i-2)*V3(3*i-2)                    &
     &                          + D(9*i-1)*V3(3*i-1)                    &
     &                          + D(9*i  )*V3(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine add_diagonal_3x33
!
!  ---------------------------------------------------------------------
!
      end module cal_4_diagonal_33
