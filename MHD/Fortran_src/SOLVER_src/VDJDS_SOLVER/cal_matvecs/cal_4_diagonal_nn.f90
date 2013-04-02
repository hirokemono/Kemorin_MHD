!
!      module cal_4_diagonal_nn
!
!     Written by Kemorin
!
!       subroutine set_by_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,       &
!     &           S, V, D)
!       subroutine set_by_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,     &
!     &           S1, S2, S3, V1, V2, V3, D)
!
!       subroutine subtract_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,     &
!     &           S, B, V, D)
!       subroutine subtract_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,   &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
!       subroutine add_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,          &
!     &           S, B, V, D)
!       subroutine add_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,        &
!     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
      module cal_4_diagonal_nn
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
       subroutine set_by_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,        &
     &           S, V, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
       real(kind = kreal), parameter :: zero = 0.0d0
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S,V,B,D,STACKmcG) tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,B)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S(ii) = zero
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,V,B,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S(ii) = S(ii) + D(im) * V(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine set_by_diagonal_nn
!
!  ---------------------------------------------------------------------
!
       subroutine set_by_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,      &
     &           S1, S2, S3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V1(NB*NP)
       real(kind = kreal), intent(in) :: V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP)
       real(kind = kreal), intent(inout) :: S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
       real(kind = kreal), parameter :: zero = 0.0d0
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,B1,B2,B3,D,STACKmcG)
!poption tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,B1,B2,B3)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S1(ii) = zero
            S2(ii) = zero
            S3(ii) = zero
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) + D(im) * V1(ix)
              S2(ii) = S2(ii) + D(im) * V2(ix)
              S3(ii) = S3(ii) + D(im) * V3(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do

!
       end subroutine set_by_diagonal_3xnn
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,      &
     &           S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(NB*NP), B(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S,V,B,D,STACKmcG) tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,B)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S(ii) = B(ii)
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,V,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S(ii) = S(ii) - D(im) * V(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine subtract_diagonal_nn
!
!  ---------------------------------------------------------------------
!
       subroutine subtract_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,    &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V1(NB*NP), B1(NB*NP)
       real(kind = kreal), intent(in) :: V2(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP), B3(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP)
       real(kind = kreal), intent(inout) :: S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,B1,B2,B3,D,STACKmcG)
!poption tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,B1,B2,B3)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S1(ii) = B1(ii)
            S2(ii) = B2(ii)
            S3(ii) = B3(ii)
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) - D(im) * V1(ix)
              S2(ii) = S2(ii) - D(im) * V2(ix)
              S3(ii) = S3(ii) - D(im) * V3(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine subtract_diagonal_3xnn
!
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_nn(NP, NB, PEsmpTOT, STACKmcG,           &
     &           S, B, V, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V(NB*NP), B(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S,V,B,D,STACKmcG) tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,B)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S(ii) = B(ii)
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S,V,B,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S(ii) = S(ii) + D(im) * V(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine add_diagonal_nn
!
!  ---------------------------------------------------------------------
!
       subroutine add_diagonal_3xnn(NP, NB, PEsmpTOT, STACKmcG,         &
     &           S1, S2, S3, B1, B2, B3, V1, V2, V3, D)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
!
       real(kind = kreal), intent(in) :: V1(NB*NP), B1(NB*NP)
       real(kind = kreal), intent(in) :: V2(NB*NP), B2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP), B3(NB*NP)
       real(kind = kreal), intent(in) :: D(NB*NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP)
       real(kind = kreal), intent(inout) :: S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: ip, iS, iE, i, k1, k2, ii, ix, im
!
!
!cdir parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!$omp parallel do private(iS,iE,i,k1,k2,ii,ix,im) 
!poption indep (S1,S2,S3,V1,V2,V3,B1,B2,B3,D,STACKmcG)
!poption tlocal (iS,iE,i,k1,k2,ii,ix,im) 
      do ip= 1, PEsmpTOT

        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,B1,B2,B3)
          do i= iS, iE
            ii = NB*(i-1) + k1
            S1(ii) = B1(ii)
            S2(ii) = B2(ii)
            S3(ii) = B3(ii)
          end do
        end do

        do k1 = 1, NB
          do k2 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep noloopchg
!voption indep (S1,S2,S3,V1,V2,V3,D)
            do i= iS, iE
              ii =    NB*(i-1)    + k1
              ix =    NB*(i-1)             + k2
              im = NB*NB*(i-1) + NB*(k1-1) + k2
              S1(ii) = S1(ii) + D(im) * V1(ix)
              S2(ii) = S2(ii) + D(im) * V2(ix)
              S3(ii) = S3(ii) + D(im) * V3(ix)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
       end subroutine add_diagonal_3xnn
!
!  ---------------------------------------------------------------------
!
      end module cal_4_diagonal_nn
