!
!      module ordering_by_new2old_U_nn
!
!     Written by Kemorin
!
!       subroutine ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT,          &
!     &           STACKmcG, NtoO_U, X, A)
!       subroutine ordering_nx2_by_new2old_U(NP, NB, PEsmpTOT,          &
!     &           STACKmcG, NtoO_U, X1, X2, A1, A2)
!       subroutine ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT,          &
!     &           STACKmcG, NtoO_U, X1, X2, X3, A1, A2, A3)
!
      module ordering_by_new2old_U_nn
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
       subroutine ordering_nx1_by_new2old_U(NP, NB, PEsmpTOT,           &
     &           STACKmcG, NtoO_U, X, A)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A(NB*NP)
       real(kind = kreal), intent(inout) :: X(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,ii,k1)
!$omp parallel do private(iS,iE,i,in1,ii,k1)
!poption indep (X,A,NtoO_U,STACKmcG) tlocal (iS,iE,i,in1,ii,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X,A,NtoO_U)
          do i= iS, iE
              ii =  NB*(    i - 1    ) + k1
              in1 = NB*( NtoO_U(i)-1 ) + k1
            X(in1)= A(ii)
          end do
        enddo
      enddo
!$omp end parallel do

       end subroutine ordering_nx1_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx2_by_new2old_U(NP, NB, PEsmpTOT,           &
     &           STACKmcG, NtoO_U, X1, X2, A1, A2)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind = kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,ii,k1)
!$omp parallel do private(iS,iE,i,in1,ii,k1)
!poption indep (X1,X2,X3,A1,A2,A3,NtoO_U,STACKmcG)
!poption tlocal (iS,iE,i,in1,ii,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,A3,NtoO_U)
          do i= iS, iE
              ii =  NB*(    i - 1    ) + k1
              in1 = NB*( NtoO_U(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
          end do
        end do
      end do
!$omp end parallel do

       end subroutine ordering_nx2_by_new2old_U
!
!  ---------------------------------------------------------------------
!
       subroutine ordering_nx3_by_new2old_U(NP, NB, PEsmpTOT,           &
     &           STACKmcG, NtoO_U, X1, X2, X3, A1, A2, A3)
!
       integer(kind = kint), intent(in) :: NP, NB, PEsmpTOT
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: NtoO_U(NP)
       real(kind = kreal), intent(in) :: A1(NB*NP), A2(NB*NP)
       real(kind = kreal), intent(in) :: A3(NB*NP)
       real(kind = kreal), intent(inout) :: X1(NB*NP), X2(NB*NP)
       real(kind = kreal), intent(inout) :: X3(NB*NP)
!
      integer (kind = kint) :: ip, iS, iE, i, in1, ii, k1
!
!
!cdir parallel do private(iS,iE,i,in1,ii,k1)
!$omp parallel do private(iS,iE,i,in1,ii,k1)
!poption indep (X1,X2,X3,A1,A2,A3,NtoO_U,STACKmcG)
!poption tlocal (iS,iE,i,in1,ii,k1)
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do k1 = 1, NB
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (X1,X2,X3,A1,A2,A3,NtoO_U)
          do i= iS, iE
              ii =  NB*(    i - 1    ) + k1
              in1 = NB*( NtoO_U(i)-1 ) + k1
            X1(in1)= A1(ii)
            X2(in1)= A2(ii)
            X3(in1)= A3(ii)
          end do
        end do
      end do
!$omp end parallel do

       end subroutine ordering_nx3_by_new2old_U
!
!  ---------------------------------------------------------------------
!
      end module ordering_by_new2old_U_nn
