!
!      module ordering_by_o2nl_nn
!
!     Written by Kemorin
!
!       subroutine ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,&
!     &           OtoN_L, X, A)
!       subroutine ordering_nx2_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,&
!     &           OtoN_L, X1, X2, A1, A2)
!       subroutine ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,&
!     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!       subroutine ordering_nx4_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,&
!     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!       subroutine ordering_nx6_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG,&
!     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,           &
!     &           A4, A5, A6)
!
      module ordering_by_o2nl_nn
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
       subroutine ordering_nx1_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG, &
     &           OtoN_L, X, A)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A(NB*NP)
       real(kind=kreal), intent(inout) :: X(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,k1,ii)
!$omp parallel do private(iS,iE,i,in1,k1,ii)
!poption indep (A,X,OtoN_L,STACKmcG) tlocal (iS,iE,i,in1,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (A,X,OtoN_L)
          do i= iS, iE
            ii    = NB*(    i - 1    ) + k1
            in1   = NB*( OtoN_L(i)-1 ) + k1
            X(in1)= A(ii)
          enddo
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_nx1_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx2_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG, &
     &           OtoN_L, X1, X2, A1, A2)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind=kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,k1,ii)
!$omp parallel do private(iS,iE,i,in1,k1,ii)
!poption indep (A1,A2,X1,X2,OtoN_L,STACKmcG) tlocal (iS,iE,i,in1,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (A1,A2,X1,X2,OtoN_L)
          do i= iS, iE
            ii    = NB*(    i - 1    ) + k1
            in1   = NB*( OtoN_L(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
          end do
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_nx2_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx3_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG, &
     &           OtoN_L, X1, X2, X3, A1, A2, A3)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind=kreal), intent(in) :: A3(NB*NP)
       real(kind=kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
       real(kind=kreal), intent(inout) :: X3(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,k1,ii)
!$omp parallel do private(iS,iE,i,in1,k1,ii)
!poption indep (A1,A2,A3,X1,X2,X3,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (A1,A2,A3,X1,X2,X3,OtoN_L)
          do i= iS, iE
            ii    = NB*(    i - 1    ) + k1
            in1   = NB*( OtoN_L(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
            X3(in1)= A3(ii)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine ordering_nx3_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx4_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG, &
     &           OtoN_L, X1, X2, X3, X4, A1, A2, A3, A4)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind=kreal), intent(in) :: A3(NB*NP), A4(NB*NP)
       real(kind=kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
       real(kind=kreal), intent(inout) :: X3(NB*NP), X4(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,k1,ii)
!$omp parallel do private(iS,iE,i,in1,k1,ii)
!poption indep (A1,A2,A3,A4,X1,X2,X3,X4,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (A1,A2,A3,A4,X1,X2,X3,X4,OtoN_L)
          do i= iS, iE
            ii    = NB*(    i - 1    ) + k1
            in1   = NB*( OtoN_L(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
            X3(in1)= A3(ii)
            X4(in1)= A4(ii)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine ordering_nx4_by_old2new_L
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx6_by_old2new_L(NP, NB, PEsmpTOT, STACKmcG, &
     &           OtoN_L, X1, X2, X3, X4, X5, X6, A1, A2, A3,            &
     &           A4, A5, A6)
!
       integer (kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind=kint), intent(in) :: OtoN_L(NP)
!
       real(kind=kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind=kreal), intent(in) :: A3(NB*NP), A4(NB*NP)
       real(kind=kreal), intent(in) :: A5(NB*NP), A6(NB*NP)
       real(kind=kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
       real(kind=kreal), intent(inout) :: X3(NB*NP), X4(NB*NP)
       real(kind=kreal), intent(inout) :: X5(NB*NP), X6(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,k1,ii)
!$omp parallel do private(iS,iE,i,in1,k1,ii)
!poption indep (A1,A2,A3,A4,A5,A6,X1,X2,X3,X4,X5,X6,OtoN_L,STACKmcG)
!poption tlocal (iS,iE,i,in1,k1,ii)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (A1,A2,A3,A4,A5,A6,X1,X2,X3,X4,X5,X6,OtoN_L)
          do i= iS, iE
            ii    = NB*(    i - 1    ) + k1
            in1   = NB*( OtoN_L(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
            X3(in1)= A3(ii)
            X4(in1)= A4(ii)
            X5(in1)= A5(ii)
            X6(in1)= A6(ii)
          end do
        enddo
      enddo
!$omp end parallel do
!
       end subroutine ordering_nx6_by_old2new_L
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_o2nl_nn
