!
!      module cal_4_diagonal_33d
!
!     Written by Kemorin
!
!       subroutine set_by_diagonal_33d(NP, PEsmpTOT, STACKmcG, S, V, D)
!       subroutine set_by_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,        &
!     &           S1, S2, S3, V1, V2, V3, D)
!
!       subroutine subtract_diagonal_33d(NP, PEsmpTOT, STACKmcG,        &
!     &           S, B, V, D)
!       subroutine subtract_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,      &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
!       subroutine add_diagonal_33d(NP, PEsmpTOT, STACKmcG,             &
!     &           S, B, V, D)
!       subroutine add_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,           &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
      module cal_4_diagonal_33d
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
       subroutine set_by_diagonal_33d(NP, PEsmpTOT, STACKmcG, S, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S,V,D,STACKmcG) tlocal (iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,V,D)
        do i= iS, iE
          S(3*i-2)= D(9*i-8)*V(3*i-2)
          S(3*i-1)= D(9*i-4)*V(3*i-1)
          S(3*i  )= D(9*i  )*V(3*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine set_by_diagonal_33d
!
!  ---------------------------------------------------------------------
!
       subroutine set_by_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,         &
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
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S1,S2,S3,V1,V2,V3,D,STACKmcG) tlocal (iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,V1,V2,V3,D)
        do i= iS, iE
          S1(3*i-2)= D(9*i-8)*V1(3*i-2)
          S2(3*i-2)= D(9*i-8)*V2(3*i-2)
          S3(3*i-2)= D(9*i-8)*V3(3*i-2)
          S1(3*i-1)= D(9*i-4)*V1(3*i-1)
          S2(3*i-1)= D(9*i-4)*V2(3*i-1)
          S3(3*i-1)= D(9*i-4)*V3(3*i-1)
          S1(3*i  )= D(9*i  )*V1(3*i  )
          S2(3*i  )= D(9*i  )*V2(3*i  )
          S3(3*i  )= D(9*i  )*V3(3*i  )
        enddo
      enddo
!$omp end parallel do
!
       end subroutine set_by_diagonal_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_33d(NP, PEsmpTOT, STACKmcG,         &
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
!
!
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S,B,V,D,STACKmcG) tlocal (iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,B,V,D)
        do i= iS, iE
          S(3*i-2) = B(3*i-2) - D(9*i-8) * V(3*i-2)
          S(3*i-1) = B(3*i-1) - D(9*i-4) * V(3*i-1)
          S(3*i  ) = B(3*i  ) - D(9*i  ) * V(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine subtract_diagonal_33d
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,       &
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
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S1,S2,S3,B1,B2,B3,V1,V2,V3,D,STACKmcG) tlocal (iS,iE,i)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,B1,B2,B3,V1,V2,V3,D)
        do i= iS, iE
          S1(3*i-2) = B1(3*i-2) - D(9*i-8) * V1(3*i-2)
          S2(3*i-2) = B2(3*i-2) - D(9*i-8) * V2(3*i-2)
          S3(3*i-2) = B3(3*i-2) - D(9*i-8) * V3(3*i-2)
          S1(3*i-1) = B1(3*i-1) - D(9*i-4) * V1(3*i-1)
          S2(3*i-1) = B2(3*i-1) - D(9*i-4) * V2(3*i-1)
          S3(3*i-1) = B3(3*i-1) - D(9*i-4) * V3(3*i-1)
          S1(3*i  ) = B1(3*i  ) - D(9*i  ) * V1(3*i  )
          S2(3*i  ) = B2(3*i  ) - D(9*i  ) * V2(3*i  )
          S3(3*i  ) = B3(3*i  ) - D(9*i  ) * V3(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine subtract_diagonal_3x33d
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_33d(NP, PEsmpTOT, STACKmcG, S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: B(3*NP), V(3*NP)
       real(kind = kreal), intent(in) :: D(9*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: ip, iS, iE, i
!
!
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S,B,V,D,STACKmcG) tlocal (iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S,B,V,D)
        do i= iS, iE
          S(3*i-2) = B(3*i-2) + D(9*i-8) * V(3*i-2)
          S(3*i-1) = B(3*i-1) + D(9*i-4) * V(3*i-1)
          S(3*i  ) = B(3*i  ) + D(9*i  ) * V(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine add_diagonal_33d
!
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_3x33d(NP, PEsmpTOT, STACKmcG,            &
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
!cdir parallel do private(iS,iE,i) 
!$omp parallel do private(iS,iE,i) 
!poption indep (S1,S2,S3,B1,B2,B3,V1,V2,V3,D,STACKmcG)
!poption tlocal (iS,iE,i) 
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (S1,S2,S3,B1,B2,B3,V1,V2,V3,D)
        do i= iS, iE
          S1(3*i-2) = B1(3*i-2) + D(9*i-8) * V1(3*i-2)
          S2(3*i-2) = B2(3*i-2) + D(9*i-8) * V2(3*i-2)
          S3(3*i-2) = B3(3*i-2) + D(9*i-8) * V3(3*i-2)
          S1(3*i-1) = B1(3*i-1) + D(9*i-4) * V1(3*i-1)
          S2(3*i-1) = B2(3*i-1) + D(9*i-4) * V2(3*i-1)
          S3(3*i-1) = B3(3*i-1) + D(9*i-4) * V3(3*i-1)
          S1(3*i  ) = B1(3*i  ) + D(9*i  ) * V1(3*i  )
          S2(3*i  ) = B2(3*i  ) + D(9*i  ) * V2(3*i  )
          S3(3*i  ) = B3(3*i  ) + D(9*i  ) * V3(3*i  )
        enddo
      enddo
!$omp end parallel do

!
       end subroutine add_diagonal_3x33d
!
!  ---------------------------------------------------------------------
!
      end module cal_4_diagonal_33d
